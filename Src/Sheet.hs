module Src.Sheet where


import Data.Maybe

import Control.Monad
import Control.Concurrent.STM

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Aeson                  as JSON

readonly :: Attr Element Bool
readonly = fromJQueryProp "readonly" (== JSON.Bool True) JSON.Bool

data Expr = Var String -- placeholder
          deriving Show

type Cell = Either Expr String

type Pos = (Int, Int)

posAdd :: Pos -> Pos -> Pos
posAdd (r1,c1) (r2,c2) = (r1+r2,c1+c2)

data Sheet = Sheet { sheetOffset    :: Pos,
                     sheetCells     :: Map Pos Cell,
                     sheetFocus     :: Pos,
                     sheetIns       :: [[(Pos, (Element, Element))]] }

sliceList :: Int -> Int -> [a] -> [a]
sliceList from to xs = take (to - from + 1) (drop from xs)

subLists :: Int -> [a] -> [[a]]
subLists i xs = let is = [0,i..(length xs - 1)]
                in map (\i' -> sliceList i' (i'+i-1) xs) is

initSheet :: UI Sheet
initSheet = do
  let cols = 5
  let rows = 3
--  cells <- replicateM (rows*cols) (UI.input # set readonly True # set UI.bgcolor "#00000")
  cells <- replicateM (rows*cols) UI.input
  shells <- replicateM (rows*cols) UI.button
  let shelledCells = zip shells cells
  let cells' = map (\(rI,r) -> zip [(rI,cI) | cI <- [0..cols]] r)
             $ zip [0..rows]
             $ subLists cols shelledCells
  return $ Sheet (0, 0) Map.empty (0, 0) cells'

focusSheetIn :: TVar Sheet -> UI ()
focusSheetIn ctxSh
  = do
  sh <- liftIO $ atomically $ readTVar ctxSh
  let focusIn = sheetFocus sh
  UI.setFocus ((fst . snd) (sheetIns sh !! fst focusIn !! snd focusIn))

moveFocus :: TVar Sheet -> Pos -> UI ()
moveFocus ctxSh dPos
  = do
  sh <- liftIO $ atomically $ readTVar ctxSh
  liftIO $ atomically $ writeTVar ctxSh (sh {sheetFocus = dPos `posAdd` sheetFocus sh})
  focusSheetIn ctxSh


-- Unsafe operation, will crash if an invalid position is given
getSheetIn :: Pos -> Sheet -> Element
getSheetIn (r,c) sh
  = (snd . snd) $ sheetIns sh !! r !! c

cells2Ins :: TVar Sheet -> UI ()
cells2Ins ctxSh
  = do
  sh <- liftIO $ atomically $ readTVar ctxSh
  mapM_ (\(p, (_,elm)) -> cell2In (sheetCells sh) (p `posAdd` sheetOffset sh) elm) (concat $ sheetIns sh)
  --liftIO $ atomically $ writeTVar ctxSh (sh {sheetIns = })

cell2In :: Map Pos Cell -> Pos -> Element -> UI ()
cell2In cs pos elm
  = do
  let cCnt = Map.findWithDefault (Right "") pos cs
  case cCnt of
    Left expr -> element elm # set UI.value (show expr)
    Right str -> element elm # set UI.value str
  return ()

scrollSheet :: TVar Sheet -> Pos -> UI ()
scrollSheet ctxSh dPos
  = do
  sh <- liftIO $ atomically $ readTVar ctxSh
  liftIO $ atomically $ writeTVar ctxSh (sh {sheetOffset = dPos `posAdd` sheetOffset sh})
  cells2Ins ctxSh
  return ()
