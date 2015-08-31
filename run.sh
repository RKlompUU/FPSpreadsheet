#!/bin/bash

. BashStd.sh

function compilationFailed {
  return $(confirm 'Compilation failed!
Want to run the old binary?')
}

function run {
  noIArguments=$(filterString "$*" -i)
  if anyString "$*" -i; then
    ghci Src/Main.hs
  else
    ./bin/Main $noIArguments
  fi
}

function compileAndRun {
  if ./make.sh; then
    run $@
  else
    compilationFailed && run $@
  fi
}

noMArgs=$(filterString "$*" -m)
if anyString "$*" -m; then
  compileAndRun $noMArgs
else
  run $noMArgs
fi
