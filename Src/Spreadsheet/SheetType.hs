module Src.Spreadsheet.SheetType where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map

import Graphics.UI.Threepenny.Core

import Src.Lambda.ExprParser

data Cell = Cell { text :: String
                 , lTerm :: Maybe (LC String) }

type Pos = (Int, Int)

data Sheet = Sheet { sheetOffset    :: Pos,
                     sheetCells     :: Map Pos Cell,
                     sheetFocus     :: Pos,
                     sheetIns       :: [[(Pos, (Element, Element))]],
                     sheetColNs     :: [Element],
                     sheetRowNs     :: [Element] }
