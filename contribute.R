#!/usr/bin/env Rscript

# Please run this script before you submit your Pull Request on GitHub.
#
# Usage:
#   In the R console: source("contribute.R")
#   In the terminal: Rscript contribute.R
#   In RStudio: Click on Source (Ctrl/Cmd-Shift-s)
#
# Thanks for contributing to workflowr!

# Print warnings as they occur
options(warn = 1)

# Check required packages ------------------------------------------------------

message("* Checking that required dependencies are installed")
pkgs <- c("devtools", "git2r", "roxygen2", "spelling", "stringr", "testthat", "withr")
installed <- rownames(installed.packages())

for (p in pkgs) {
  if (!p %in% installed) {
    msg <- sprintf("%s is required. Install with install.packages(\"%s\")", p, p)
    stop(msg, call. = FALSE)
  }
}

# Check setup ------------------------------------------------------------------

message("* Checking setup")

if (!file.exists("workflowr.Rproj")) {
  stop("Please run this script from the root directory.",
       "\nUse setwd() or in RStudio Click Session -> Set Working Directory -> To Project Directory",
       call. = FALSE)
}

if (!git2r::in_repository()) {
  stop("Something went wrong with the Git repository", call. = FALSE)
}

r <- git2r::repository()
b <- git2r::repository_head(r)$name
if (b == "master") {
  warning("You are currently on the master branch.",
          " Please switch to the dev branch with:",
          "\n\ngit checkout dev")
}

remote_avail <- git2r::remotes(r)
if (!"origin" %in% remote_avail) {
  warning("There is no remote named origin")
} else {
  origin <- git2r::remote_url(r, "origin")
  if (stringr::str_detect(origin, "jdblischak/workflowr.git$")) {
    warning("The remote origin is set to the main workflowr repository.",
            "\nDid you fork the repository on GitHub and then clone your fork?")
  }
}

# Update documentation ---------------------------------------------------------

message("* Updating documentation")
devtools::document(roclets = c('rd', 'collate', 'namespace'))

# Check package ----------------------------------------------------------------

message("* Checking package")
devtools::check(document = FALSE, args = "--no-tests", error_on = "error")
