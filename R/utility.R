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
  if (length(path) > 1) stop("Invalid input: the vector should only have element")
  if (is.null(path)) stop("Invalid input: NULL")
  if (is.na(path)) stop("Invalid input: NA")
  if (!is.character(path)) stop("Invalid input: ", path)

  if (fs::dir_exists(path)) {
    return(absolute(path))
  } else {
    return(obtain_existing_path(dirname(path)))
  }
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

  newpath <- path
  # Convert to absolute path
  newpath <- fs::path_abs(newpath)
  # Expand ~ using R's definition of user directory
  newpath <- fs::path_expand_r(newpath)
  # Ensure Windows Drive is uppercase
  newpath <- toupper_win_drive(newpath)
  # Resolve symlinks
  if (.Platform$OS.type == "windows") {
    newpath <- resolve_symlink(newpath)
  } else {
    newpath <- fs::path_real(newpath)
  }
  newpath <- as.character(newpath)

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

  newpath <- path
  # First resolve any symlinks
  newpath <- absolute(newpath)
  start <- absolute(start)

  # Handle any issues with Windows drives
  if (.Platform$OS.type == "windows") {
    # Require that all files are on the same Windows drive
    drive <- unique(get_win_drive(newpath))
    drive <- drive[!is.na(drive)]
    if (length(drive) != 1) {
      stop("All paths must be on the same Windows drive", call. = FALSE)
    }
    # If path and start are on different drives, return the absolute path
    drive_start <- get_win_drive(start)
    if (drive != drive_start) return(newpath)
  }

  # Convert to relative path
  newpath <- fs::path_rel(newpath, start = start)
  newpath <- as.character(newpath)

  return(newpath)
}

# Resolve symlinks in a filepath even if file does not exist.
#
# Input: Vector of absolute filepaths
# Output: Vector of absolute filepaths with any symlinks resolved
resolve_symlink <- function(path) {
  return(vapply(path, resolve_symlink_, character(1), USE.NAMES = FALSE))
}

# Recursive function to resolve symlinks one path at a time.
resolve_symlink_ <- function(path) {
  # Base case #1: If path exists, resolve symlink
  if (fs::file_exists(path)) {
    return(fs::path_real(path))
  }

  parts <- fs::path_split(path)[[1]]
  len <- length(parts)

  # Base case #2: Only 1 part of file path remaining. Return it.
  #
  # Possible cases:
  #   * Invalid input such as NA
  #   * A Fake file path that doesn't exist on the machine
  if (len == 1) {
    return(path)
  }

  # Recursive case
  return(fs::path_join(c(
    resolve_symlink_(fs::path_join(parts[-len])),
    parts[len])))
}

# Attempts to delete file(s) and/or directory(ies) pointed to by path.
#
# path - character vector
#
# Fails gracefully. unlink() fails silently. fs::file_delete() properly throws
# an error, but it is not the most informative. This will report the files that
# weren't deleted as well as their file permissions.
wflow_delete <- function(path) {

  # Needed for, e.g. AppVeyor, where relative paths can cause permission issues
  path <- absolute(path)

  attempt_to_delete <- try(fs::file_delete(path), silent = TRUE)

  persistent <- fs::file_exists(path)
  if (any(persistent)) {
    stop("Unable to delete the following file(s) or directory(ies):\n",
         paste(path[persistent], collapse = "\n"),
         "\nDo you have permission to delete them? The permissions are, in order:\n",
         paste(fs::file_info(path[persistent])$permissions, collapse = "\n"),
         call. = FALSE)
  }

  return(invisible(path))
}

# Because ~ maps to ~/Documents on Windows, need a reliable way to determine the
# user's home directory on Windows.
# https://cran.r-project.org/bin/windows/base/rw-FAQ.html#What-are-HOME-and-working-directories_003f
# https://stat.ethz.ch/pipermail/r-help/2007-March/128221.html
# https://github.com/ropensci/git2r/pull/320#issuecomment-367038961
get_home <- function() {
  # If non-Windows, it is straightforward
  if (.Platform$OS.type != "windows") {
    home <- "~"
    return(absolute(home))
  } else {
    home <- Sys.getenv("USERPROFILE")
    home <- absolute(home)
    if (!fs::dir_exists(home)) {
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

# If the user doesn't define a URL for a host repo in the YAML header or
# _workflowr.yml, determine the URL from the remote "origin". If this remote
# doesn't exist, return NA.
#
# GitHub:
# HTTPS: https://github.com/jdblischak/workflowr.git
# SSH: git@github.com:jdblischak/workflowr.git
# Return value:  https://github.com/jdblischak/workflowr
#
# GitLab:
# HTTPS: https://gitlab.com/jdblischak/wflow-gitlab.git
# SSH: git@gitlab.com:jdblischak/wflow-gitlab.git
# Return value: https://gitlab.com/jdblischak/wflow-gitlab
get_host_from_remote <- function(path) {
  if (!git2r::in_repository(path = path)) {
    return(NA_character_)
  }
  r <- git2r::repository(path = path, discover = TRUE)
  remotes <- git2r::remotes(r)
  if (!("origin" %in% remotes)) {
    return(NA_character_)
  }
  origin <- git2r::remote_url(r, remote = "origin")
  host <- origin
  # Remove trailing .git
  host <- stringr::str_replace(host, "\\.git$", "")
  # If SSH, replace with HTTPS URL
  host <- stringr::str_replace(host, "^git@(.+):", "https://\\1/")
  return(host)
}

# Get output directory if it exists
get_output_dir <- function(directory, yml = "_site.yml") {

  stopifnot(fs::dir_exists(directory))

  site_fname <- file.path(directory, "_site.yml")
  if (!fs::file_exists(site_fname)) {
    return(NULL)
  }
  site_yml <- yaml::yaml.load_file(site_fname)

  if (is.null(site_yml$output_dir)) {
    output_dir <- directory
  } else {
    output_dir <- file.path(directory, site_yml$output_dir)
    fs::dir_create(output_dir)
    output_dir <- absolute(output_dir)
  }

  return(output_dir)
}

# Convert the output of git2r::status() to a data frame for easier manipulation
status_to_df <- function(x) {
  stopifnot(class(x) == "git_status")

  col_status <- character()
  col_substatus <- character()
  col_file <- character()

  for (stat in names(x)) {
    files <- unlist(x[[stat]])
    col_status <- c(col_status, rep(stat, length(files)))
    col_substatus <- c(col_substatus, names(files))
    col_file <- c(col_file, files)
  }

  out <- data.frame(status = col_status,
                    substatus = col_substatus,
                    file = col_file,
                    row.names = seq_along(col_status),
                    stringsAsFactors = FALSE)
  return(out)
}

# Convert data frame to git_status
df_to_status <- function(d) {
  stopifnot(is.data.frame(d),
            colnames(d) == c("status", "substatus", "file"))
  status <- list(staged = structure(list(), .Names = character(0)),
                 unstaged = structure(list(), .Names = character(0)),
                 untracked = structure(list(), .Names = character(0)))
  for (i in seq_along(d$file)) {
    status[[d$status[i]]] <- c(status[[d$status[i]]], list(d$file[i]))
    names(status[[d$status[i]]])[length(status[[d$status[i]]])] <- d$substatus[i]
  }
  class(status) <- "git_status"
  return(status)
}

# Determine if a file is executable
#
# https://github.com/r-lib/fs/issues/172
file_is_executable <- function(f) {
  stopifnot(fs::file_exists(f))

  if (fs::file_access(f, mode = "execute")) return(TRUE)

  return(FALSE)
}

# Automatically try to determine the best setting for knitr's dependson chunk
# option based on the caching status of other chunks in the document.
#
# Usage: Set dependson=workflowr:::wflow_dependson() for a given chunk
#
# Desired behavior:
#   * If any other cached chunks are invalidated and re-executed, this chunk
#     should also be re-executed.
#   * If all the cached chunks in a document are read from the cache, also read
#     this chunk from the cache.
#   * Avoid as many knitr warnings as possible about depending on non-cached
#     chunks. These are harmless, but avoiding these demonstrates that the
#     function is using dependson as it is intended to be used.
#
# Long term goal: Use this with the sessionInfo() chunk inserted by workflowr
#
# Warning: knitr caching is complicated. Make sure to test this function's
# behavior for your setup before relying on it for anything important.
#
# https://yihui.name/knitr/options/#cache
# https://yihui.name/knitr/demo/cache/
# https://stackoverflow.com/a/47055058/2483477
# https://stackoverflow.com/questions/25436389/dependson-option-does-not-work-in-knitr
#
wflow_dependson <- function() {

  cache_global <- knitr::opts_chunk$get("cache")

  # If cache=TRUE is set globally, depend on all chunks except those that are
  # explicitly labeled cache=FALSE.
  if (cache_global) {
    labels_all <- knitr::all_labels()
    labels_cache_false <- knitr::all_labels(expression(cache == FALSE))
    labels <- setdiff(labels_all, labels_cache_false)
    # Remove label of current chunk
    label_self <- knitr::opts_current$get(name = "label")
    labels <- setdiff(labels, label_self)
    # Remove the set.seed chunk inserted by workflowr b/c it can't be cached
    labels <- setdiff(labels, "seed-set-by-workflowr")
    if (length(labels) > 0) return(labels)
  }

  # If specific chunks are cached, depend on these
  labels_cache_true <- knitr::all_labels(expression(cache == TRUE))
  if (length(labels_cache_true) > 0) return(labels_cache_true)

  # If the document doesn't use caching, don't use dependson.
  return(NULL)
}

# Ensure that Windows drive is capitalized
#
# Motivation: getwd() on winbuilder returns d:/ but tempdir() returns D:/. This
# causes problems when creating relative paths.
toupper_win_drive <- function(path) {
  stringr::str_replace(path, "^([a-z]):/", toupper)
}

# Return the Windows drive. Called by relative().
#
# > get_win_drive(c("C:/a/b/c", "D:/a/b/c"))
# [1] "C:" "D:"
get_win_drive <- function(path) {
  drive <- fs::path_split(path)
  drive <- vapply(drive, function(x) x[1], FUN.VALUE = character(1))
  return(drive)
}

# Return TRUE if getOption("browser") is properly set. Required for opening URLs
# via browseURL().
#
# This can either be an R function that accepts a URL or a string with the
# name of the system program to invoke (e.g. "firefox"). If it is NULL or "",
# it won't work.
check_browser <- function() {
  browser_opt <- getOption("browser")

  if (is.null(browser_opt)) return(FALSE)

  if (is.function(browser_opt)) return(TRUE)

  if (nchar(browser_opt) > 0) return(TRUE)

  return(FALSE)
}

# Only return the first line of a multi-line string(s)
get_first_line <- function(x) {
  split <- stringr::str_split(x, "\n")
  first_lines <- vapply(split, function(x) x[1], character(1))
  return(first_lines)
}

# Check for `site: workflowr::wflow_site` in index.Rmd
check_site_generator <- function(index) {
  if (!fs::file_exists(index))
    stop(glue::glue("Unable to find index.Rmd. Expected to find {index}"),
         call. = FALSE)

  header <- rmarkdown::yaml_front_matter(index)

  if (is.null(header$site)) return(FALSE)

  if (header$site == "workflowr::wflow_site") return(TRUE)

  return(FALSE)
}

is_rmd <- function(path) {
  extensions <- fs::path_ext(path)
  stringr::str_detect(extensions, "^[Rr]md$")
}

# Save the files open in RStudio editor
autosave <- function() {
  if (!rstudioapi::isAvailable(version_needed = "1.1.287")) return(FALSE)

  rstudioapi::documentSaveAll()
}

check_wd_exists <- function() {
  wd <- fs::path_wd()
  if (length(fs::path_wd()) == 0)
    stop("The current working directory doesn't exist.",
         " Use setwd() to change to an existing directory.",
         call. = FALSE)
}
