module Sheet where

import Control.Monad
import Graphics.UI.WX hiding (Event)

data Expr = Var String -- placeholder

data Sheet = Sheet [[Expr]]


sliceList :: Int -> Int -> [a] -> [a]
sliceList from to xs = take (to - from + 1) (drop from xs)

subLists :: Int -> [a] -> [[a]]
subLists i xs = let is = [0,i..(length xs - 1)]
                in map (\i' -> sliceList i' (i'+i-1) xs) is

--visibleCells :: IO
visibleCells f = do
  let rows = 4
  let cols = 4
  cells <- replicateM (rows*cols) (entry f [])
  return (subLists rows cells)
