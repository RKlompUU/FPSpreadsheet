{-|
Module      : SpreedSheet.Sheet
Description : An experimental application of the spreadsheet API
Stability   : experimental
-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Spreadsheet.Sheet
      ( module Spreadsheet.Sheet
      , module Spreadsheet.SheetType
      , module Lambda.Lambda
      ) where

import Spreadsheet.SheetType

import Data.Maybe

import Control.Monad
import Control.Concurrent.STM

import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Aeson                  as JSON

import API.SheetAbstr

import Debug.Trace

import Lambda.Lambda

type CellCntTy = LExpr String

instance Spreadsheet (Sheet CellCntTy) (CellT CellCntTy) CellCntTy String (State (Sheet CellCntTy)) where
  updateEvals = do
              s <- get
              mapM_ updateEval ((Map.toList s))
  getCell p = do
            s <- get
            return (Map.lookup p s)
  setCell s p c = Map.insert p c s

updateEval :: (Pos, CellT CellCntTy) -> State (Sheet CellCntTy) ()
updateEval (p, c) =
  do
    s <- get
    mC <- getCell p
    let mC' = mC >>= \c -> return $ parseCell c
        globVars = maybe []
                         (mapMaybe (\p -> mC' >>= getEval >>= \e -> trace ("cRefs: " ++ show p) return (cRefPos2Var p, e)) . scanCellRefs)
                         (mC' >>= \c' -> getEval c' >>= return . lExpr_)
        mC'' = trace ("test: " ++ show globVars) mC' >>= \c' -> return $ evalCell (setGlobalVars c' globVars)
        oldEval = mC >>= \c -> getEval c >>= return . lExpr_
        newEval = mC'' >>= \c'' -> getEval c'' >>= return . lExpr_
    if (oldEval == newEval)
      then return ()
      else let c'' = case newEval of
                       Just e -> trace ("Updating e to: " ++ show e) c {uFlag = True, lExpr = Just $ LExpr e Map.empty}
                       Nothing -> c {uFlag = True, lExpr = Nothing}
               s' = setCell s p c''
           in put s' >> updateEvals


instance Cell (CellT CellCntTy) CellCntTy String where
  evalCell c@CellT {lExpr = maybeE} =
    case maybeE of
      Just e  -> c {lExpr = Just $ (evalExpr e)}
      Nothing -> c
  parseCell c@CellT {Spreadsheet.SheetType.text = code} =
    c {lExpr = flip LExpr Map.empty <$> parseExpr code}
  getEval = lExpr
  getText = Spreadsheet.SheetType.text
  setGlobalVars c@CellT {lExpr = maybeE} defs =
    case maybeE of
      Just e  -> c {lExpr = Just $ foldr (\(v,def) e' -> addGlobalVar e' v def) (cleanGlobalVars e) defs}
      Nothing -> c

readonly :: UI.Attr UI.Element Bool
readonly = UI.fromJQueryProp "readonly" (== JSON.Bool True) JSON.Bool

-- | 'isInBox' if the position is inside the box 'Nothing' is returned.
-- Otherwise, it returns the offset of the position towards the box.
isInBox :: Pos -> (Pos,Pos) -> Maybe Pos
isInBox (r,c) ((rL, cL), (rH, cH))
  = let rOffset = if r < rL
                    then r - rL
                    else if r > rH
                      then r - rH
                      else 0
        cOffset = if c < cL
                    then c - cL
                    else if c > cH
                      then c - cH
                      else 0
    in if rOffset == 0 && cOffset == 0
        then Nothing
        else Just (rOffset,cOffset)

-- | 'grabUpdatedCells' filters out all cells that have not changed.
grabUpdatedCells :: (Var v, Expr e v) => Sheet e -> Sheet e
grabUpdatedCells = Map.filter uFlag

-- | 'resetUpdateFields' removes the update flags of all cells.
resetUpdateFields :: (Var v, Expr e v) => Sheet e -> Sheet e
resetUpdateFields = Map.map (\c -> c {uFlag = False})

-- | Subtraction on 'Pos' variables.
posSubtr :: Pos -> Pos -> Pos
posSubtr (r1,c1) (r2,c2) = (r1-r2,c1-c2)

-- | Addition on 'Pos' variables.
posAdd :: Pos -> Pos -> Pos
posAdd (r1,c1) (r2,c2) = (r1+r2,c1+c2)

-- | 'sliceList' grabs a part of list 'xs' that ranges from index 'from' to
-- index 'to'.
sliceList :: Int -> Int -> [a] -> [a]
sliceList from to xs = take (to - from + 1) (drop from xs)

-- | 'subLists' slices a list 'xs', where each slice has a length of at most
-- 'i'.
subLists :: Int -> [a] -> [[a]]
subLists i xs = let is = [0,i..(length xs - 1)]
                in map (\i' -> sliceList i' (i'+i-1) xs) is

initSheet :: (Var v, Expr e v) => Sheet e
initSheet = Map.empty

-- | Helper function to conveniently obtain a 'CellT e' from the 'Sheet e'.
getSheetCell :: (Var v, Expr e v) => Pos -> Sheet e -> CellT e
getSheetCell pos cs
  = Map.findWithDefault emptyCell pos cs

emptyCell :: (Var v, Expr e v) => CellT e
emptyCell = CellT "" Nothing False

-- | 'scanCellRefs' obtains all references that are present in an
-- expression. This is for example used to find out which global variables
-- need to be added to an expression prior to evaluating it.
scanCellRefs :: LC v -> [Pos]
scanCellRefs (CVar p)    = [p]
scanCellRefs (Lam _ e)   = scanCellRefs e
scanCellRefs (App e1 e2) = scanCellRefs e1 ++ scanCellRefs e2
scanCellRefs _ = []
