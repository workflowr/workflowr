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
  ext <- tools::file_ext(files)
  if (!all(stringr::str_detect(ext, "[Rr]md$")))
      stop("Invalid file extension")
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

# Return a relative version of a path
#
# This is a port of the Python function os.path.relpath. I couldn't find an
# equivalent. If you know of an available function in a lightweight dependency,
# please let me know.
#
# Description from Python docs: Return a relative filepath to path either from
# the current directory or from an optional start directory. This is a path
# computation: the filesystem is not accessed to confirm the existence or nature
# of path or start.
#
# Note on this implementation:
#  * Not vectorized.
#  * Expects absolute paths with no tilde.
#
# https://docs.python.org/3.5/library/os.path.html#os.path.relpath
relpath <- function(path, start = NULL) {
  if (!(is.character(path) & length(path) == 1))
    stop("path must be a character vector")
  if (!(is.null(start) | is.character(start) & length(start) == 1))
    stop("start must be NULL or a 1-element character vector")

  curdir <- "."
  sep <- .Platform$file.sep
  pardir <- ".."

  if (is.null(start))
    start <- getwd()

  start_list <- unlist(stringr::str_split(start, sep))
  path_list <- unlist(stringr::str_split(path, sep))
  # Work out how much of the filepath is shared by start and path.
  i <- length(commonprefix(list(start_list, path_list)))

  rel_list <- c(rep(pardir, length(start_list) - i),
                path_list[(i + 1):length(path_list)])
  if (length(rel_list) == 0)
    return(curdir)
  return(paste(rel_list, collapse = sep))
}

# "Given a list of pathnames, returns the longest common leading component"
commonprefix <- function(m) {
  if (length(m) == 0)
    return("")
  path_lengths <- sapply(m, length)
  s1 <- m[[which.min(path_lengths)]]
  s2 <- m[[which.max(path_lengths)]]
  for (i in seq_along(s1)) {
    if (s1[i] != s2[i])
      return(s1[1:(i - 1)])
  }
  return(s1)
}
