#!/usr/bin/env Rscript

cat("Installing remotes\n")
if (!requireNamespace("remotes", quietly = TRUE))
  install.packages("remotes")

cat("Installing outdated dependencies\n")
remotes::install_deps(dependencies = TRUE)
