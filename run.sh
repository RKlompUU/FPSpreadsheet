#!/bin/bash

. BashStd.sh

exec 2>&1

function compilationFailed {
  return $(confirm 'Compilation failed!
Want to run the old binary?')
}

function run {
  noIArguments=$(filterString "$*" -i)
  if anyString "$*" -i; then
    debugModule=`choice "$(find Src -iname *.hs)"`
    cabal exec ghci -- $debugModule
  else
    ./bin/Main $noIArguments <&0
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
