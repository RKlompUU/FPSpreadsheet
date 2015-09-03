{-----------------------------------------------------------------------------
    reactive-banana-wx

    Example: Very simple arithmetic
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

module Main where

import Data.Maybe

import Data.Char

import Control.Monad (void)
import Control.Concurrent.STM

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
--import Reactive.Banana

-- wx wrapper: heinrich apfelmus
-- sodium library?
import qualified Data.Aeson                  as JSON

import qualified Text.Blaze.Html as HTML
import qualified Text.Blaze.Html.Renderer.String as HTML

import Src.Sheet

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

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main
  = do
  startGUI defaultConfig setup
  return ()


getSheet :: TVar Sheet -> UI Sheet
getSheet ctxSh = liftIO $ atomically $ readTVar ctxSh


getHtml :: JSFunction String
getHtml = ffi "document.documentElement.innerHTML"

-- | Focus an element.
isFocused :: Element -> UI Bool
isFocused elm
  = do
  retVal <- callFunction $ ffi "$(%1).is(':focus')" elm
  return (if retVal == "true" then True else False)

setup :: Window -> UI ()
setup rootWindow
  = do
  sheet <- initSheet

  ctxSh <- liftIO $ atomically $ newTVar sheet

  debugField <- UI.paragraph # set UI.text "Test"
  return rootWindow # set UI.title "Hello World!"

  rootWindowBody <- UI.getBody rootWindow
  on UI.keydown rootWindowBody (toKeyCodeM (rootKeyHandler ctxSh debugField))

  mapM_ (\cell -> on UI.keydown (grabCell cell) (toKeyCodeM $ sheetMod ctxSh rootWindow debugField cell))
        (concat $ sheetIns sheet)

  mapM_ (\cell -> on UI.keydown (grabShell cell) (toKeyCodeM $ shellKeyHandler debugField ctxSh cell))
        (concat $ sheetIns sheet)

  getBody rootWindow #+
    [  grid $ (map . map)
              (\(_,(shell,cell)) -> element shell #+ [element cell])
              (sheetIns sheet)
    , element debugField ]

  UI.setFocus (grabShell $ (head . head) (sheetIns sheet))

  return ()

-- sheet modification
sheetMod :: TVar Sheet -> Window -> Element -> (Pos, (Element,Element)) -> KeyCode -> UI ()
--sheetMod ctxSh rootWindow debugField (inPos,(inShell,inCell))
sheetMod ctxSh rootWindow debugField (inPos,(inShell,inCell)) KeyCodeEnter
  = do
  -- Save edited content in the spreadsheet, and exit input focus
  sh <- getSheet ctxSh
  cCnt <- get UI.value (getSheetIn inPos sh)
  cPos <- getAbsoluteCPos ctxSh inPos
  let sh' = cellMod cCnt cPos sh
  liftIO $ atomically $ writeTVar ctxSh sh'
--  UI.setFocus inShell
--  element debugField # set UI.text (show (sheetOffset sh))
sheetMod ctxSh rootWindow debugField (inPos,(inShell,inCell)) KeyCodeEsc
  = do
  sh <- getSheet ctxSh
  cPos <- getAbsoluteCPos ctxSh inPos
  cell2In (sheetCells sh) cPos inCell
sheetMod ctxSh rootWindow debugField (inPos,(inShell,inCell)) k
  = do
  element debugField # set UI.text (show k)
  --dumpHtml rootWindow debugField
  return ()

cellMod :: String -> Pos -> Sheet -> Sheet
cellMod cCnt cPos sh
  = sh {
      sheetCells = Map.insert cPos (Right cCnt) (sheetCells sh)
    }

shellKeyHandler :: Element -> TVar Sheet -> (Pos, (Element,Element)) -> KeyCode -> UI ()
shellKeyHandler _ ctxSh (cPos, (cShell,cCell)) KeyCodeEnter
  = do
  cellHasFocus <- isFocused cCell
  if cellHasFocus
    then UI.setFocus cShell
    else UI.setFocus cCell
shellKeyHandler _ ctxSh (cPos, (cShell,cCell)) KeyCodeEsc = UI.setFocus cShell
shellKeyHandler debugField ctxSh (cPos, (cShell,cCell)) k
  | any (==k) [KeyCodeUp, KeyCodeDown, KeyCodeLeft, KeyCodeRight]
      = do
      cellHasFocus <- isFocused cCell
      if cellHasFocus
        then return ()
        else moveFocus ctxSh (key2Dir k)
  | otherwise
      = element debugField # set UI.text (show k) >> return ()

rootKeyHandler :: TVar Sheet -> Element -> KeyCode -> UI ()
rootKeyHandler ctxSh debugField _ = return ()

dumpHtml :: Window -> Element -> UI ()
dumpHtml rootWindow debugField
  = do
  htmlCode <- callFunction getHtml
  -- This prettyfi stuff isn't actually working :/
  let prettyHtmlCode = HTML.renderHtml
                     $ HTML.toHtml htmlCode
  element debugField # set UI.text htmlCode
  return ()
