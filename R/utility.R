# Non-exported utility functions
#
# See tests/testthat/test-utility.R for usage examples.

# Obtain the most upstream existing path.
#
# normalizePath only returns the absolute path if the directory exists. It is
# often useful to expand a potential path to an absolute path for debugging and
# error handling. This function returns the most upstream existing path as an
# absolute path.
#
# Currently it is used by `wflow_start` to check for the presence of an upstream
# Git repository before creating a new project directory.
#
# path - a path to a file or directory. Can be relative or absolute, existing or
# non-existing.
#
obtain_existing_path <- function(path) {
  if (path == "") {
    return(normalizePath("."))
  } else if (dir.exists(path)) {
    return(normalizePath(path))
  } else {
    parts <- stringr::str_split(path, pattern = .Platform$file.sep)[[1]]
    newpath <- paste(parts[-length(parts)],
                     collapse = .Platform$file.sep)
    # print(newpath)
    return(obtain_existing_path(newpath))
  }
}

# Run diff between two files.
#
# tools::Rdiff runs `diff` between two files. Unfortunately it sends messages
# with `cat`, which makes it difficult to control its output programmatically.
# This is a simple wrapper that returns the results as a character vector.
#
diff_file <- function(from, to) {
  ignore <- utils::capture.output(
    diffs <- tools::Rdiff(from = from, to = to, Log = TRUE))
  return(diffs$out)
}

# Wrap long messages
# https://github.com/jdblischak/workflowr/issues/29
wrap <- function(...) {
  input <- list(...)
  if (!all(sapply(input, is.character)))
    stop("All input must be a character vector")
  m <- paste(unlist(input), collapse = "")
  paste(strwrap(m), collapse = "\n")
}

# Convert R Markdown file to corresponding HTML
to_html <- function(files, outdir = NULL) {
  html <- stringr::str_replace(files, "[Rr]md$", "html")
  if (!is.null(outdir)) {
    # Remove trailing slash if present
    if (stringr::str_sub(outdir, nchar(outdir), nchar(outdir)) == "/") {
      outdir <- stringr::str_sub(outdir, 1, nchar(outdir) - 1)
    }
    html <- file.path(outdir, basename(html))
  }
  return(html)
}
