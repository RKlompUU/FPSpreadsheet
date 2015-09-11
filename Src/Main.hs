{-----------------------------------------------------------------------------
    reactive-banana-wx

    Example: Very simple arithmetic
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

module Main where

import Graphics.UI.Threepenny.Core

import Src.Lambda.Lambda
import Src.UI.UI

import Data.Maybe
import System.IO
import System.Environment
import Control.Monad

import Src.Lambda.Test

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main
  = do
  args <- getArgs
  let cmds = case mapMaybe (\arg -> maybe Nothing
                                          (\v -> Just (arg,v))
                                          (lookup arg arg2Func))
                           args of
              []    -> arg2Func -- If no commands have explicitly been given,
              cmds' -> cmds'    -- then simply execute everything
  fileInput <- hIsSeekable stdin
  if fileInput
    then runTests cmds
    else startGUI defaultConfig setup
  return ()

arg2Func = lambdaTests

runTests cmds
  = do
  src <- getContents
  let abstr = parseExpr src
  mapM_ (\(l,f)  -> putStrLn " |"
         >>= (\_ -> putStrLn "\\ /")
         >>= (\_ -> putStrLn " `")
         >>= (\_ -> putStrLn (l ++ replicate (80 - length l) '*'))
         >>= (\_ -> f abstr)
         >>= (\_ -> putStrLn (replicate 80 '*'))) cmds
