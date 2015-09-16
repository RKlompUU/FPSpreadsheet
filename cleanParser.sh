#!/bin/sh

ps=`find Src -iname *.y`
ls=`find Src -iname *.x`

for l in $ls; do
  woExt=`echo $l | cut -f 1 -d '.'`
  rm "${woExt}.hs"
  rm build/.$(basename $l).tL
done

for p in $ps; do
  woExt=`echo $p | cut -f 1 -d '.'`
  rm "${woExt}.hs"
  rm build/.$(basename $p).tP
done
