module Src.Spreadsheet.SheetType where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map

import Graphics.UI.Threepenny.Core

import Src.Lambda.ExprParser
import Src.Lambda.IdInt

data Cell = Cell { text  :: String
                 , lExpr :: Maybe (LC IdInt)
                 , uFlag :: Bool -- Cell has changed, used to check if an input field needs to be refreshed
                 }

type Pos = (Int, Int)

data Sheet = Sheet { sheetOffset    :: Pos,
                     sheetCells     :: Map Pos Cell,
                     sheetFocus     :: Pos,
                     sheetIns       :: [[(Pos, (Element, Element))]],
                     sheetColNs     :: [Element],
                     sheetRowNs     :: [Element] }

sheetInSize :: Sheet -> (Pos, Pos)
sheetInSize Sheet {sheetColNs = cs, sheetRowNs = rs} = ((0,0), (length rs - 1, length cs - 1))
