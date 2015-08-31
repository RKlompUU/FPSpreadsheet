module Src.Sheet where

import Control.Monad
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

data Expr = Var String -- placeholder

type Cell = Either Expr String

type Pos = (Int, Int)

data Sheet = Sheet { sheetOffset    :: Pos,
                     sheetCells     :: [[Cell]],
                     sheetIns       :: [[(Pos, TextCtrl ())]] }


sliceList :: Int -> Int -> [a] -> [a]
sliceList from to xs = take (to - from + 1) (drop from xs)

subLists :: Int -> [a] -> [[a]]
subLists i xs = let is = [0,i..(length xs - 1)]
                in map (\i' -> sliceList i' (i'+i-1) xs) is

--getAttrTest :: w -> IO a
-- getAttrTest w = get

initSheet :: Window a -> IO Sheet
initSheet f = do
  let cols = 5
  let rows = 3
  cells <- replicateM (rows*cols) (entry f [])
  let cells' = map (\(rI,r) -> zip [(rI,cI) | cI <- [0..cols]] r)
             $ zip [0..rows]
             $ subLists cols cells
  return $ Sheet (0, 0) (repeat $ repeat (Right "")) cells'
