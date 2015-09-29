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

type Sheet = Map Pos Cell
