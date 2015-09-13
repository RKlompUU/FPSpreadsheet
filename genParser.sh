#!/bin/bash

ps=`find Src -iname *.y`
ls=`find Src -iname *.x`

for l in $ls; do
  tStampF=build/.$(basename $l).tL
  t1=""
  t2="45"
  if [ -f $tStampF ]; then
    t1=`cat $tStampF`
    t2=`stat -c %y "$l"`
  fi

  if [ "$t1" != "$t2" ]; then
    alex $l
    echo `stat -c %y "$l"` > $tStampF
    hF="${l/Lexer\.x/Parser\.y}"
    tStampF=build/.$(basename $hF).tP
    if [ -f $hF ]; then
      happy $hF
      echo `stat -c %y "$hF"` > $tStampF
    fi
  fi
done

for p in $ps; do
  tStampF=build/.$(basename $p).tP

  t1=""
  t2="45"
  if [ -f $tStampF ]; then
    t1=`cat $tStampF`
    t2=`stat -c %y "$p"`
  fi

  if [ "$t1" != "$t2" ]; then
    echo `stat -c %y "$p"` > $tStampF
    happy $p
  fi
done
