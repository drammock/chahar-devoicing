#! /bin/bash

## create TeX file
pandoc -s --latex-engine=xelatex --natbib --no-tex-ligatures --template=template-abstract-anonymous.tex -o LSA-abstract.tex LSA-abstract.md

## run xelatex, bibtex8, xelatex, xelatex, xelatex
. create.sh LSA-abstract.tex

## delete second page (references)
pdftk LSA-abstract.pdf cat 1 output LSA-abs-final.pdf

## vacuum metadata
exiftool -Author= -Creator= -Producer= LSA-abs-final.pdf
