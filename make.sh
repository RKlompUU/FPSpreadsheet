#!/bin/bash

. BashStd.sh

# $1: file extension
# $2: from directory
# $3: to directory
enterDir() {
  ext=$1
  fD=$2
  tD=$3
  if [ ! -e $fD ]; then
    echo "$fD doesn't exist"
    return
  fi

  mkdir -p $tD

  files=$(ls $fD)
  extFiles=$(filterFileExtensions "$files" $ext)
  dirFiles=$(filterDirs "$files" $fD)

  for f in $extFiles; do
    mv "$fD/$f" "$tD/$f"
  done

  for d in $dirFiles; do
    enterDir $ext "$fD/$d" "$tD/$d"
    ext=$1
    fD=$2
    tD=$3
  done
}


./genParser.sh

enterDir hi build Src
enterDir o  build Src

cabal exec ghc -- --make -O Src/Main.hs 2>&1

enterDir hi Src build
enterDir o  Src build

mkdir -p bin
mv Src/Main bin/Main
