#!/usr/bin/env Rscript

# This is a convenience script for when I'm developing the package. The main
# issue is that building the package in RStudio (Ctrl+Shift+B) or documenting
# the package (Ctrl+Shift+D) builds the vignettes and pollutes `vignettes/` with
# the generated `*R` and `*html` files instead of putting them in `inst/doc/`.
# This script ensures the documentation is up-to-date and also removes
# unnecessary files.

stopifnot(file.exists("workflowr.Rproj"))

devtools::document()
devtools::clean_vignettes(); devtools::build_vignettes(); devtools::clean_vignettes()
devtools::install(dependencies = FALSE)

unlink("docs/articles/*R")
pkgdown::build_site()

# Use LICENSE.html. Backporting this feature to older version of pkgdown b/c I
# get error from the latest version. See
# https://github.com/r-lib/pkgdown/issues/363
unlink("docs/LICENSE")
index_in <- readLines("docs/index.html")
index_out <- sub('<a href="LICENSE">LICENSE</a>',
                 '<a href="LICENSE.html">LICENSE</a>',
                 index_in)
writeLines(index_out, "docs/index.html")
