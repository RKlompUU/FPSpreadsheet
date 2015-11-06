#!/bin/sh

latex final.tex
dvipdf final.dvi final.pdf
evince final.pdf &
