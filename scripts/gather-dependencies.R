#!/usr/bin/env Rscript

# Create the vector `dependencies` in R/zzz.R

if (!file.exists("DESCRIPTION")) {
  stop("gather-dependencies.R must be executed in the root directory of the package")
}

imports <- read.dcf("DESCRIPTION", fields = "Imports")[[1]]
imports <- strsplit(imports, split = ",\\n")[[1]]
packages <- stringr::str_extract(imports, "^[:alpha:][:alnum:]+")
versions <- stringr::str_extract(imports, "[:digit:][[:digit:]\\.-]+")
dependencies <- versions
names(dependencies) <- packages

copy_paste <- utils::capture.output(dput(dependencies))
copy_paste <- paste(c("dependencies <- ", copy_paste), collapse = "")
copy_paste <- gsub(",", ",\n", copy_paste)
cat(copy_paste)
