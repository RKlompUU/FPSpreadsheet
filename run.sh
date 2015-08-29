#!/bin/bash

. BashStd.sh

function compilationFailed {
  return $(confirm 'Compilation failed!
Want to run the old binary?')
}

function run {
  ./bin/Main $@
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
  compileAndRun noMArgs
else
  run noMArgs
fi
