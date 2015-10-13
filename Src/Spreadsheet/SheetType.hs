module Src.Spreadsheet.SheetType
  ( module Src.API.SheetAbstr
  , module Src.Spreadsheet.SheetType ) where


import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map

import Graphics.UI.Threepenny.Core

import Src.Lambda.ExprParser
import Src.Lambda.IdInt
import Src.Lambda.Lambda


import Src.API.SheetAbstr

data CellT e = CellT { text  :: String
                     , lExpr :: Maybe (e)
                     , uFlag :: Bool -- Cell has changed, used to check if an input field needs to be refreshed
                     }


type Sheet e = Map Pos (CellT e)


