module Src.Spreadsheet.SheetType where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map

import Graphics.UI.Threepenny.Core

import Src.Lambda.ExprParser

data Cell = Cell { text :: String
                 , lTerm :: Maybe (LC String) }

type Pos = (Int, Int)

type Sheet = Map Pos Cell
