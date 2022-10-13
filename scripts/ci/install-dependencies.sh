#!/bin/bash
set -eu

# Install dependencies with APT/r2u

# Imports
imports="\
  r-cran-callr \
  r-cran-fs \
  r-cran-getpass \
  r-cran-git2r \
  r-cran-glue \
  r-cran-httpuv \
  r-cran-httr \
  r-cran-knitr \
  r-cran-rmarkdown \
  r-cran-rprojroot \
  r-cran-rstudioapi \
  r-cran-stringr \
  r-cran-whisker \
  r-cran-xfun \
  r-cran-yaml \
"

# Suggests
suggests="\
  r-cran-clipr \
  r-cran-miniui \
  r-cran-reticulate \
  r-cran-sessioninfo \
  r-cran-shiny \
  r-cran-testthat \
  r-cran-withr \
"

# SystemRequirements
system="\
  pandoc \
  pandoc-citeproc \
"

# R CMD check
check="\
  qpdf \
"

# Coverage
coverage="\
  r-cran-covr \
"

# Install
apt-get install --yes $imports $suggests $system $check $coverage

# List installed R packages
apt list --installed 'r-*'
