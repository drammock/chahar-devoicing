#! /bin/bash

pandoc -s --latex-engine=xelatex --natbib --no-tex-ligatures --template=template-abstract-anonymous.tex -o LSA-abstract.tex LSA-abstract.md

. create.sh LSA-abstract.tex
