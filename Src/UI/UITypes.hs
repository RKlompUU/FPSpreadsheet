module Src.UI.UITypes where

import Src.Spreadsheet.Sheet -- Kind of ugly that this needs to be imported... (for type Pos)
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

data KeyCode
  = KeyCodeLeft
  | KeyCodeRight
  | KeyCodeUp
  | KeyCodeDown
  | KeyCodeEnter
  | KeyCodeEsc
  | KeyCodeSpecial Int
  deriving (Show,Eq)

toKeyCode :: UI.KeyCode -> KeyCode
toKeyCode 13 = KeyCodeEnter
toKeyCode 27 = KeyCodeEsc
toKeyCode 37 = KeyCodeLeft
toKeyCode 38 = KeyCodeUp
toKeyCode 39 = KeyCodeRight
toKeyCode 40 = KeyCodeDown
toKeyCode k  = KeyCodeSpecial k

toKeyCodeM :: (KeyCode -> UI a) -> UI.KeyCode -> UI a
toKeyCodeM f = f . toKeyCode

key2Dir :: KeyCode -> Pos
key2Dir KeyCodeLeft  = (0,-1)
key2Dir KeyCodeUp    = (-1,0)
key2Dir KeyCodeRight = (0,1)
key2Dir KeyCodeDown  = (1,-0)
key2Dir _            = (0,0)


data UISheet = UISheet { sheetCells  :: Sheet (LExpr String)
                       , sheetCursor :: Pos
                       , sheetOffset :: Pos
                       , sheetIns    :: [[(Pos, (Element, Element))]]
                       , sheetColNs  :: [Element]
                       , sheetRowNs  :: [Element] }
