#!/bin/bash
# Double-click this file in Finder to render moral_numbers.Rmd to PDF

# Change to the script's directory so relative paths work
cd "$(dirname "$0")"

# Remove stale .aux file to avoid biblatex/citeproc conflicts
rm -f moral_numbers.aux

Rscript -e "rmarkdown::render('moral_numbers.Rmd', output_format = bookdown::pdf_document2(keep_tex = TRUE))"

# Full recompilation to resolve bibliography and cross-references
xelatex -interaction=nonstopmode moral_numbers.tex
bibtex moral_numbers
xelatex -interaction=nonstopmode moral_numbers.tex
xelatex -interaction=nonstopmode moral_numbers.tex
