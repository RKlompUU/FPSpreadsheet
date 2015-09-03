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
  | KeyCodeSpecial Int

toKeyCode :: UI.KeyCode -> KeyCode
toKeyCode 37 = KeyCodeLeft
toKeyCode 38 = KeyCodeUp
toKeyCode 39 = KeyCodeRight
toKeyCode 40 = KeyCodeDown
toKeyCode k  = KeyCodeSpecial k

toKeyCodeM :: (KeyCode -> UI a) -> UI.KeyCode -> UI a
toKeyCodeM f = f . toKeyCode

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main
  = do
  startGUI defaultConfig setup
  return ()


getHtml :: JSFunction String
getHtml = ffi "document.documentElement.innerHTML"

setup :: Window -> UI ()
setup rootWindow
  = do
  sheet <- initSheet

  ctxSh <- liftIO $ atomically $ newTVar sheet

  debugField <- UI.paragraph # set UI.text "Test"
  return rootWindow # set UI.title "Hello World!"

  rootWindowBody <- UI.getBody rootWindow
  on UI.keydown rootWindowBody (toKeyCodeM (rootKeyHandler ctxSh debugField))

  mapM_ (\cell -> on UI.keydown (grabCell cell) (toKeyCodeM $ sheetMod ctxSh rootWindow debugField (grabPos cell)))
        (concat $ sheetIns sheet)

  getBody rootWindow #+
    [  grid $ (map . map)
              (\(_,(shell,cell)) -> element shell #+ [element cell])
              (sheetIns sheet)
    , element debugField ]

  UI.setFocus (grabCell $ (head . head) (sheetIns sheet))
  UI.setFocus rootWindowBody

  return ()

-- sheet modification
sheetMod :: TVar Sheet -> Window -> Element -> Pos -> KeyCode -> UI ()
sheetMod ctxSh rootWindow debugField p@(r, c) k_
  = do
  sh <- liftIO $ atomically $ readTVar ctxSh
  cCnt <- get UI.value (getSheetIn p sh)
  let cPos = p `posAdd` sheetOffset sh
  let sh' = cellMod cCnt cPos sh
  liftIO $ atomically $ writeTVar ctxSh sh'
  element debugField # set UI.text (show (sheetOffset sh))
  dumpHtml rootWindow debugField
  return ()

cellMod :: String -> Pos -> Sheet -> Sheet
cellMod cCnt cPos sh
  = sh {
      sheetCells = Map.insert cPos (Right cCnt) (sheetCells sh)
    }

--shellKeyHandler ::

rootKeyHandler :: TVar Sheet -> Element -> KeyCode -> UI ()
rootKeyHandler ctxSh debugField KeyCodeLeft  = moveFocus ctxSh (0,-1)
rootKeyHandler ctxSh debugField KeyCodeUp    = moveFocus ctxSh (-1,0)
rootKeyHandler ctxSh debugField KeyCodeRight = moveFocus ctxSh (0,1)
rootKeyHandler ctxSh debugField KeyCodeDown  = moveFocus ctxSh (1,-0)
rootKeyHandler ctxSh debugField _ = return ()

dumpHtml :: Window -> Element -> UI ()
dumpHtml rootWindow debugField
  = do
  htmlCode <- callFunction getHtml
  let prettyHtmlCode = HTML.renderHtml
                     $ HTML.toHtml htmlCode
  element debugField # set UI.text htmlCode
  return ()
