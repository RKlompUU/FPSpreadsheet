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

import Src.Sheet

keyCodeLeft :: Num a => a
keyCodeLeft = 37
keyCodeUp :: Num a => a
keyCodeUp = 38
keyCodeRight :: Num a => a
keyCodeRight = 39
keyCodeDown :: Num a => a
keyCodeDown = 40

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main
  = do
  startGUI defaultConfig setup
  return ()

setup :: Window -> UI ()
setup rootWindow
  = do
  sheet <- initSheet

  ctxSh <- liftIO $ atomically $ newTVar sheet

  upButton <- UI.button #+ [ string "Up" ]
  on UI.click upButton (\_ -> scrollSheet ctxSh (-1,0))
  downButton <- UI.button #+ [ string "Down" ]
  on UI.click downButton (\_ -> scrollSheet ctxSh (1,0))
  leftButton <- UI.button #+ [ string "Left" ]
  on UI.click leftButton (\_ -> scrollSheet ctxSh (0,-1))
  rightButton <- UI.button #+ [ string "Right" ]
  on UI.click rightButton (\_ -> scrollSheet ctxSh (0,1))

  debugField <- UI.paragraph # set UI.text "Test"
  return rootWindow # set UI.title "Hello World!"

  rootWindowBody <- UI.getBody rootWindow
  on UI.keydown rootWindowBody (rootKeyHandler ctxSh debugField)


  mapM_ (\cell -> on UI.keydown (snd . snd $ cell) (sheetMod ctxSh rootWindow debugField (fst cell)))
        (concat $ sheetIns sheet)

  getBody rootWindow #+
    [  grid $ (map . map)
              (\(_,(shell,cell)) -> element shell #+ [element cell]) 
              (sheetIns sheet)
    , element debugField ]

  UI.setFocus (snd . snd $ (sheetIns sheet !! 0 !! 0))
  UI.setFocus rootWindowBody

  return ()

-- sheet modification
sheetMod :: TVar Sheet -> Window -> Element -> Pos -> UI.KeyCode -> UI ()
sheetMod ctxSh rootWindow debugField p@(r, c) k_
  = do
  sh <- liftIO $ atomically $ readTVar ctxSh
  cCnt <- get UI.value (getSheetIn p sh)
  let cPos = p `posAdd` sheetOffset sh
  let sh' = cellMod cCnt cPos sh
  liftIO $ atomically $ writeTVar ctxSh sh'
  element debugField # set UI.text (show (sheetOffset sh))
  return ()
  where k = chr k_

cellMod :: String -> Pos -> Sheet -> Sheet
cellMod cCnt cPos sh
  = sh {
      sheetCells = Map.insert cPos (Right cCnt) (sheetCells sh)
    }


rootKeyHandler :: TVar Sheet -> Element -> UI.KeyCode -> UI ()
rootKeyHandler ctxSh debugField 37 {-left-}  = moveFocus ctxSh (0,-1)
rootKeyHandler ctxSh debugField 38 {-up-}    = moveFocus ctxSh (-1,0)
rootKeyHandler ctxSh debugField 39 {-right-} = moveFocus ctxSh (0,1)
rootKeyHandler ctxSh debugField 40 {-down-}  = moveFocus ctxSh (1,-0)
rootKeyHandler ctxSh debugField _ = return ()

dumpHtml :: Element -> Element -> UI ()
dumpHtml rootWindow debugField
  = do
  text <- get UI.html rootWindow
  return ()
