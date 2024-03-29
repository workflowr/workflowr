#!/usr/bin/env Rscript

# Bump package version
#
# Usage: Rscript scripts/bump-version.R <next-version>
#
# Bumps the version in the following files:
#
# * DESCRIPTION
# * NEWS.md

# Setup ------------------------------------------------------------------------

stopifnot(file.exists("workflowr.Rproj"))

library(stringr)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 1) {
  next_version <- args[1]
} else {
  stop("Usage: Rscript scripts/bump-version.R <next-version>")
}

# DESCRIPTION ------------------------------------------------------------------

# can't use read.dcf() because write.dcf() mangles the output
message("* Updating DESCRIPTION")
lines_in_desc <- readLines("DESCRIPTION")
lines_out_desc <- stringr::str_replace(lines_in_desc, "^Version.*",
                                       sprintf("Version: %s", next_version))
writeLines(lines_out_desc, "DESCRIPTION")

# NEWS.md ----------------------------------------------------------------------

message("* Updating NEWS.md")
lines_in_news <- readLines("NEWS.md")
current_version <- stringr::str_split_fixed(lines_in_news[1], " ", n = 3)[3]
is_dev <- length(stringr::str_split(current_version, "\\.")[[1]]) > 3

lines_out_news <- lines_in_news
if (is_dev) {
  lines_out_news[1] <- sprintf("# workflowr %s", next_version)
} else {
  lines_out_news <- c(sprintf("# workflowr %s", next_version), "",
                      lines_out_news)
}

writeLines(lines_out_news, "NEWS.md")
