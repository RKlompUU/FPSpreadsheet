{-----------------------------------------------------------------------------
    reactive-banana-wx

    Example: Very simple arithmetic
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

module Main where

import Data.Maybe

import Graphics.UI.SDL as SDL
import Reactive.Banana
import Reactive.Banana.SDL

-- wx wrapper: heinrich apfelmus
-- sodium library?

import Src.Sheet


bSheetAction :: Behavior t Sheet
bSheetAction
  = accumB undefined (eCellChanged <$ never)
  where eCellChanged :: Sheet -> Sheet
        eCellChanged sh = undefined

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main
  = do
  _ <- SDL.init [InitEverything]
  SDL.setVideoMode 640 480 32 []
  quitHandler
  return ()

quitHandler :: IO ()
quitHandler
  = do
  e <- waitEvent
  case e of
    Quit -> return ()
    _ -> quitHandler
