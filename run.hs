#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import System.Directory
import System.Environment
import Data.Text (pack)
import Data.List
import Control.Monad.Loops
import qualified Control.Foldl as Fold
import qualified System.IO as SystIO

import System.Process (callCommand)

compilationFailed
  = do
  print "COMPILATION FAILED"
  print "Want to run the old binary? (y/n)"
  t <- readline
  case t of
    Just inp -> return inp
    _        -> return ""

run runArgs
  = do
  callCommand ("./bin/Main " ++ intercalate " " runArgs)
  return ()

compileAndRun runArgs
  = do
    returnC <- shell "./make.hs" empty
    case returnC of
      ExitSuccess   -> do run runArgs
      ExitFailure n -> do yn <- iterateWhile (\t -> t /= pack "y" && t /= pack "n")
                                             compilationFailed
                          when (yn == "y") $ run runArgs



main
  = do
  args <- getArgs
  let noMArgs = filter (/="-m") args
  let make = any (=="-m") args
  let runArgs = noMArgs
  if make
    then compileAndRun runArgs
    else run runArgs

