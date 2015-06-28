#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (FilePath)

import System.Exit
import Turtle
import System.Directory
import System.Environment -- getArgs
import qualified Control.Foldl as Fold
import Control.Monad
import Data.Text (pack)


import qualified Filesystem.Path as FP

import Data.List

import System.Process (callCommand)

enterDir :: Text -> FilePath -> FilePath -> IO ()
enterDir f fD tD
  = do
  exists <- testdir fD
  if not exists
    then return ()
    else do
          mktree tD

          files <- fold (ls fD) Fold.list

          let fs = filter (flip hasExtension f) files
          mapM (\f -> mv (FP.append fD (FP.filename f)) (FP.append tD (FP.filename f))) fs

          dirs <- filterM testdir files
          mapM (\dir -> enterDir f dir (FP.append tD (FP.filename dir))) dirs

          return ()


compileAG :: String -> IO ()
compileAG n
  = do
  succ <- shell (pack $ "uuagc -Hdcfws --self " ++ n ++ ".ag -o ." ++ n ++ ".hs") empty
  case succ of
    ExitFailure _ -> do
                    print "uuagc errored, not starting ghc"
                    exitFailure
    _             -> do
                    agExists <- testfile (fromString $ n ++ ".hs")
                    if not agExists
                      then do
                            print (pack $ n ++ ".hs didn't exist")
                            mv (fromString $ "." ++ n ++ ".hs") (fromString $ n ++ ".hs")
                      else do
                            cmp <- fold (inshell (pack $ "cmp " ++ n ++ ".hs ." ++ n ++ ".hs | wc -l") empty) Fold.head
                            case cmp of
                              Just "0" -> do
                                            print (pack $ n ++ ".hs hasn't changed")
                                            rm (fromString $ "." ++ n ++ ".hs")
                              _        -> do
                                            print (pack $ n ++ ".hs has changed")
                                            mv (fromString $ "." ++ n ++ ".hs") (fromString $ n ++ ".hs")

main = do
  args <- getArgs
  let install = any (\v -> v == "-i" || v == "--install") args

  enterDir "hi" "build" "src"
  enterDir "o"  "build" "src"

  cd "src"

  shell "ghc --make -O Main.hs 2>&1" empty
  cd ".."

  enterDir "hi" "src" "build"
  enterDir "o" "src" "build"

  mktree "bin"
  mv "src/Main" "bin/Main"

  when install $ callCommand "sudo cp bin/Main /usr/bin/DisplayManag"



