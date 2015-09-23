module Src.Spreadsheet.SheetType where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map

import Graphics.UI.Threepenny.Core

import Src.Lambda.ExprParser
import Src.Lambda.IdInt

data Cell = Cell { text  :: String
                 , lExpr :: Maybe (LC IdInt) }

type Pos = (Int, Int)

data Sheet = Sheet { sheetOffset    :: Pos,
                     sheetCells     :: Map Pos Cell,
                     sheetFocus     :: Pos,
                     sheetIns       :: [[(Pos, (Element, Element))]],
                     sheetColNs     :: [Element],
                     sheetRowNs     :: [Element] }
