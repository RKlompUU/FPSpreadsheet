#!/bin/bash

# $1: array of choices
function choice {
  cs=$1

  while true; do
    select opt in $cs; do
      anyString "$cs" "$opt" && echo $opt && return
    done
  done
}

# $1: array
# $2: string
function filterString {
  ss=$1
  str=$2

  for s in $ss; do
    echo $s
    if [ "$s" != "$str" ]; then
       echo $s
    fi
  done
}

# $1: array
# $2: string
function anyString {
  ss=$1
  str=$2

  for s in $ss; do
    if [ "$s" == "$str" ]; then
      return 0
    fi
  done

  false
}

function getAllArgs {
  if [ "$#" -eq "0" ]; then
    echo " "
  else
    echo "$@"
  fi
}

# $1: files
# $2: extension
function filterFileExtensions {
  files=$1
  ext=$2

  for f in $files; do
    if [[ $f == *.$ext ]]; then
      echo $f
    fi
  done
}

# $1: files
# $2: prepath
function filterDirs {
  files=$1
  pre=$2

  for f in $files; do
    if [[ -d $pre/$f ]]; then
      echo $f
    fi
  done
}

function confirm {
  if [ "$#" -ge "1" ]; then
    prompt="$1 [y/N] "
  else
    prompt="Are you sure? [y/N] "
  fi
  read -r -p "$prompt" response
  case $response in
      [yY])
          true
          ;;
      *)
          false
          ;;
  esac
}
