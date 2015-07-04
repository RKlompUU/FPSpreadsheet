module Sheet where

import Control.Monad
import Graphics.UI.WX hiding (Event)

data Expr = Var String -- placeholder

type Cell = Either Expr String

data Sheet = Sheet {cells :: [[Cell]],
                    ins   :: [[TextCtrl ()]] }


sliceList :: Int -> Int -> [a] -> [a]
sliceList from to xs = take (to - from + 1) (drop from xs)

subLists :: Int -> [a] -> [[a]]
subLists i xs = let is = [0,i..(length xs - 1)]
                in map (\i' -> sliceList i' (i'+i-1) xs) is

-- initSheet ::

--visibleCells :: IO
visibleCells f = do
  let rows = 10
  let cols = 4
  cells <- replicateM (rows*cols) (entry f [])
  return $ Sheet (repeat $ repeat (Right "")) (subLists rows cells)
