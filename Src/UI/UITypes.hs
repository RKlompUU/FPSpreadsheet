{-|
Module      :
Description :
Stability   :
-}
module UI.UITypes where

import API.SheetAbstr
import Spreadsheet.Sheet

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

-- | The keycode encodings used by ThreePenny are kind of cryptic.
-- Therefore, this module works with an internal KeyCode format
data KeyCode
  = KeyCodeLeft
  | KeyCodeRight
  | KeyCodeUp
  | KeyCodeDown
  | KeyCodeEnter
  | KeyCodeEsc
  | KeyCodeSpecial Int
  deriving (Show,Eq)

-- | Maps the KeyCode type (Which is just an Int) of Threepenny to our own KeyCode datatype.
-- Note that currently only the keycodes that have been used up to now have
-- been given a KeyCode constructor. Anything that has not yet been added
-- to our own KeyCode datatype falls through to KeyCodeSpecial Int (where
-- Int is then equal to the UI.KeyCode value).
toKeyCode :: UI.KeyCode -> KeyCode
toKeyCode 13 = KeyCodeEnter
toKeyCode 27 = KeyCodeEsc
toKeyCode 37 = KeyCodeLeft
toKeyCode 38 = KeyCodeUp
toKeyCode 39 = KeyCodeRight
toKeyCode 40 = KeyCodeDown
toKeyCode k  = KeyCodeSpecial k

-- | 'toKeyCodeM' is used as a wrapper. Threepenny has functions that
-- contain (UI.KeyCode -> UI a) parameters, toKeyCodeM is simply used to
-- conviniently wrap our own KeyCode definition around those functions.
toKeyCodeM :: (KeyCode -> UI a) -> UI.KeyCode -> UI a
toKeyCodeM f = f . toKeyCode

-- | Relative mappings of spreadsheet movements
key2Dir :: KeyCode -> Pos
key2Dir KeyCodeLeft  = (0,-1)
key2Dir KeyCodeUp    = (-1,0)
key2Dir KeyCodeRight = (0,1)
key2Dir KeyCodeDown  = (1,-0)
key2Dir _            = (0,0)

-- | 'UISheet' defines the spreadsheet type. The functions in this UI
-- submodule pass a value of this datatype along in a statewise matter.
data UISheet = UISheet { sheetCells  :: Sheet (LC String)
                       , sheetCursor :: Pos
                       , sheetOffset :: Pos
                       , sheetIns    :: [[(Pos, (Element, Element))]]
                       , sheetColNs  :: [Element]
                       , sheetRowNs  :: [Element] }
