#!/bin/sh

ps=`find Src -iname *.y`
ls=`find Src -iname *.x`

for l in $ls; do
  alex "${l}"
done

for p in $ps; do
  happy "${p}"
done

