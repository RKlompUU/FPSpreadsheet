module Src.Sheet where

import Control.Monad
import Graphics.UI.WX hiding (Event)

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

-- initSheet ::

visibleCells :: Window a -> IO Sheet
visibleCells f = do
  let cols = 5
  let rows = 3
  cells <- replicateM (rows*cols) (entry f [nullAttr "test" := 4])
  let test = map (\c -> set c [nullAttr "test" := 3]) cells
  let cells' = -- (map . map) (\(p,cell) -> (p, set cell [nullAttr "index" := p]))
               map (\(rI,r) -> zip [(rI,cI) | cI <- [0..cols]] r)
             $ zip [0..rows]
             $ subLists cols cells
  return $ Sheet (0, 0) (repeat $ repeat (Right "")) cells'
