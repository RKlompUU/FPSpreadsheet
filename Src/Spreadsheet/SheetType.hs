{-|
Module      : Spreadsheet.SheetType
Description : A Sheet datatype, that contains a grid of cells
Stability   : experimental
-}
module Spreadsheet.SheetType
  ( module API.SheetAbstr
  , module Spreadsheet.SheetType ) where


import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map

import Graphics.UI.Threepenny.Core

import Lambda.ExprParser
import Lambda.IdInt
import Lambda.Lambda


import API.SheetAbstr

data CellT e = CellT { text  :: String
                     , lExpr :: Maybe (e)
                     , uFlag :: Bool -- Cell has changed, used to check if an input field needs to be refreshed by the frontend
                     }

type Sheet e = Map Pos (CellT e)


