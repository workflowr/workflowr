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
    # Only prepend outdir if it's not "." for current working directory
    if (outdir == ".") {
      html <- basename(html)
    } else {
      html <- file.path(outdir, basename(html))
    }
  }
  return(html)
}

# A vectorized form of relpath.
#
# path - a vector of paths
#
# start - a single starting path to be relative to
#
# Both path and start will be passed through normalizePath to resolve relative
# paths to existing directories.
relpath_vec <- function(path, start = NULL) {
  if (!(is.character(path) && length(path) >= 1))
    stop("path must be a character vector")
  if (!(is.null(start) || (is.character(start) && length(start) == 1)))
    stop("start must be NULL or a 1-element character vector")

  p <- normalizePath(path, mustWork = FALSE)
  if (!is.null(start)) {
    start <- normalizePath(start, mustWork = FALSE)
  }
  o <- character(length = length(p))
  for (i in seq_along(path)) {
    o[i] <- relpath(path = p[i], start = start)
  }
  return(o)
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
# https://github.com/python/cpython/blob/3.5/Lib/posixpath.py#L431
# https://github.com/python/cpython/blob/3.5/Lib/test/test_posixpath.py#L483
relpath <- function(path, start = NULL) {
  if (is.null(path)) return(NULL)
  if (is.na(path)) return(NA)
  if (!(is.character(path) && length(path) == 1))
    stop("path must be a character vector")
  if (!(is.null(start) || (is.character(start) && length(start) == 1)))
    stop("start must be NULL or a 1-element character vector")
  if (any(stringr::str_detect(c(path, start), "~")))
    stop("No tilde allowed in path or start")

  curdir <- "."
  sep <- .Platform$file.sep
  pardir <- ".."

  if (is.null(start))
    start <- getwd()

  start_list <- unlist(stringr::str_split(start, sep))[-1]
  path_list <- unlist(stringr::str_split(path, sep))[-1]
  # Work out how much of the filepath is shared by start and path.
  i <- length(commonprefix(start_list, path_list))

  rel_list <- rep(pardir, length(start_list) - i)
  if (length(path_list) > i)
    rel_list <- c(rel_list, path_list[(i + 1):length(path_list)])
  if (length(rel_list) == 0)
    return(curdir)
  return(paste(rel_list, collapse = sep))
}

# "Given a list of pathnames, returns the longest common leading component"
#
# Note: This version is only inspired by the original Python one. The Python
# version can handle a list of strings or a list of lists of strings because the
# enumerate function works on either. The unit tests are for a list of strings,
# but relpath uses a list of lists of strings. Since it is awkward to try and do
# both in R, this only works on a list of character vectors. Also, it is
# modified for the specific use case of comparing exactly two character vectors.
#
# https://docs.python.org/3.5/library/os.path.html#os.path.commonprefix
# https://github.com/python/cpython/blob/3.5/Lib/genericpath.py#L68
# https://github.com/python/cpython/blob/3.5/Lib/test/test_genericpath.py#L32
commonprefix <- function(p1, p2) {
  stopifnot(is.character(p1), is.character(p2))
  if (length(p1) == 0 || length(p2) == 0)
    return(character())
  len_min <- pmin(length(p1), length(p2))
  incommon <- character()
  for (i in seq(len_min)) {
    if (p1[i] == p2[i]) {
      incommon <- c(incommon, p1[i])
    } else {
      break
    }
  }
  return(incommon)
}

# Override default normalizePath options for working with filepaths on Windows
normalizePath <- function(path, winslash = "/", mustWork = NA) {
  p <- base::normalizePath(path = path, winslash = winslash, mustWork = mustWork)
  # On Windows **only**, NA gets appended to path. Ensure that any NAs are
  # returned as NA
  p[is.na(path)] <- NA
  return(p)
}

# Convert any instance of \\ in a Windows path to /
convert_windows_paths <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.na(x)) return(NA)

  stringr::str_replace_all(x, pattern = "\\\\", replacement = "/")
}

# Override default tempfile to not use \\ in paths on Windows. Unlike
# normalizePath, there is no argument to change this default behavior.
tempfile <- function(pattern = "file", tmpdir = tempdir(), fileext = "") {
  tfile <- base::tempfile(pattern = pattern, tmpdir = tmpdir, fileext = fileext)
  return(convert_windows_paths(tfile))
}
