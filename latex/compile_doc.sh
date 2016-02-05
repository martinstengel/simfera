#!/bin/bash

base=doc_simulator
main=${base}.tex

pdflatex $main
bibtex $base
pdflatex $main
pdflatex $main

rm -f ${base}.aux
rm -f ${base}.bbl
rm -f ${base}.blg
rm -f ${base}.dvi
rm -f ${base}.log
rm -f ${base}.toc
