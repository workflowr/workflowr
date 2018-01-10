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
  if (dir.exists(path)) {
    return(absolute(path))
  } else {
    return(obtain_existing_path(dirname(path)))
  }
}

# Run diff between two files.
#
# tools::Rdiff runs `diff` between two files. Unfortunately it sends messages
# with `cat`, which makes it difficult to control its output programmatically.
# This is a simple wrapper that returns the results as a character vector.
#
diff_file <- function(from, to) {
  # Fail gracefully if `diff` not available on Windows
  if (.Platform$OS.type == "windows") {
    if (Sys.which("diff") == "") {
      stop(call. = FALSE,
           wrap(
             "In order to use this workflowr function on Windows, you need to
             download and install Rtools available at the link below:

             https://cran.r-project.org/bin/windows/Rtools/"
             ))
    }
  }
  # diff returns exit status 1 if any differences are found. No problem on
  # Unix-alike, but Windows sends a warning message that the command exited with
  # status 1.
  suppressWarnings(
    ignore <- utils::capture.output(
      diffs <- tools::Rdiff(from = from, to = to, Log = TRUE))
  )
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
    outdir <- stringr::str_replace(outdir, "/$", "")
    # Only prepend outdir if it's not "." for current working directory
    if (outdir == ".") {
      html <- basename(html)
    } else {
      html <- file.path(outdir, basename(html))
    }
  }
  return(html)
}

# Get an absolute path while handling cross-platform filepath issues
#
# path - a vector of paths
absolute <- function(path) {
  if (is.null(path)) return(path)
  if (all(is.na(path))) return(path)

  if (!is.character(path))
    stop("path must be NULL or a character vector")

  # Using normalizePath is frustrating because of its differences on Windows,
  # but it is the easiest way to resolve symlinks. Note that it only resolves
  # symlinks if the file or directory exists.
  newpath <- normalizePath(path, winslash = "/", mustWork = FALSE)

  # On Windows **only**, NA gets appended to path. Ensure that any NAs are
  # returned as NA
  newpath[is.na(path)] <- NA

  # On Windows **only**, normalizePath doesn't strip trailing slash. This is
  # presumably due to the edge case of the homedrive, i.e. "C:/" is a valid path
  # but not "C:". If this function manually removes the trailing slash, then the
  # drive, e.g. "C:", gets returned as the current working directory.
  # Fortunately R.utils::getAbsolutePath is smarter than normalizePath (it
  # strips the trailing slash expect when the path is to the root of a drive),
  # so this does not need to be explicitly performed.

  # normalizePath does not return an absolute path for a non-existent file or
  # directory, e.g. `normalizePath("a")` returns `"a"`.
  newpath <- R.utils::getAbsolutePath(newpath)
  # The original filepaths are added as the "names" attribute when there is more
  # than one filepath. Remove them.
  attributes(newpath) <- NULL

  return(newpath)
}

# Get a relative path while handling cross-platform filepath issues
#
# path - a vector of paths
#
# start - a single starting path to be relative to
relative <- function(path, start = getwd()) {
  if (is.null(path)) return(path)
  if (all(is.na(path))) return(path)

  if (!is.character(path))
    stop("path must be NULL or a character vector")
  if (!(is.character(start) && length(start) == 1))
    stop("start must be a character vector of length 1")

  newpath <- R.utils::getRelativePath(absolute(path),
                                      relativeTo = absolute(start))
  # The original filepaths are added as the "names" attribute when there is more
  # than one filepath. Remove them.
  attributes(newpath) <- NULL

  return(newpath)
}

# Because ~ maps to ~/Documents on Windows, need a reliable way to determine the
# user's home directory on Windows.
# https://cran.r-project.org/bin/windows/base/rw-FAQ.html#What-are-HOME-and-working-directories_003f
# https://stat.ethz.ch/pipermail/r-help/2007-March/128221.html
get_home <- function() {
  # If non-Windows, it is straightforward
  if (.Platform$OS.type != "windows") {
    home <- "~"
    return(absolute(home))
  } else {
    drive <- Sys.getenv("HOMEDRIVE")
    login <- Sys.info()["login"]
    home <- file.path(drive, "Users", login)
    home <- absolute(home)
    if (!dir.exists(home)) {
      stop(wrap("Unable to determine user's home directory on Windows: ", home))
    }
    return(home)
  }
}

# Detect if a filepath contains any globbing characters: *, ?, [...]
detect_glob <- function(paths) {
  stringr::str_detect(paths, pattern = "\\*") |
    stringr::str_detect(paths, pattern = "\\?") |
    stringr::str_detect(paths, pattern = "\\[.+\\]")
}

# Perform file globbing
#
# Sys.glob works great on filepaths with globbing characters, but it's behavior
# for non-globs depends on 1) if the filepath exists, 2) if the path is to a
# file or a directory (with or without a trailing slash), and 3) which OS the
# command is run on. To avoid these edge cases, this function only runs Sys.glob
# on filepaths that contain globbing characters.
glob <- function(paths) {
  is_glob <- detect_glob(paths)
  expanded <- Map(Sys.glob, paths)
  invalid_glob <- is_glob & vapply(expanded, length, numeric(1)) == 0
  if (any(invalid_glob))
    stop("Invalid file glob: ", paths[invalid_glob][1], call. = FALSE)
  result <- ifelse(is_glob, expanded, paths)
  result <- unique(unlist(result))
  return(result)
}
