# Utility functions for backwards compatibility with git2r
# S4 <= 0.21.0
# S3 >= 0.22.0

git2r_as.data.frame <- function(x) {
  if (isS4(x)) {
    methods::as(x, "data.frame")
  } else {
    as.data.frame(x)
  }
}

git2r_as.list <- function(x) {
  if (isS4(x)) {
    methods::as(x, "list")
  } else {
    as.list(x)
  }
}

git2r_diff <- function(x1, x2) {
  if (isS4(x1)) {
    git2r::diff(x1, x2)
  } else {
    base::diff(x1, x2)
  }
}

git2r_head <- function(x) {
  if (isS4(x)) {
    utils::getFromNamespace("head", "git2r")(x)
  } else {
    utils::getFromNamespace("repository_head", "git2r")(x)
  }
}

git2r_merge <- function(x, b) {
  if (isS4(x)) {
    m <- utils::getFromNamespace("merge", "git2r")(x, b)
    if (length(m@fast_forward) == 0) m@fast_forward <- FALSE
    if (length(m@conflicts) == 0) m@conflicts <- FALSE
    if (length(m@sha) == 0) m@sha <- NA_character_
    return(m)
  } else {
    # Note to self. If dropping support for older versions of git2r, should set
    # `fail = TRUE` to clean up repository if there was a merge conflict. Better
    # yet expose this as an argument to wflow_git_pull().
    m <- base::merge(x, b)
    if (length(m$fast_forward) == 0) m$fast_forward <- FALSE
    if (length(m$conflicts) == 0) m$conflicts <- FALSE
    if (length(m$sha) == 0) m$sha <- NA_character_
    return(m)
  }
}

git2r_slot <- function(x, slotname) {
  if (isS4(x)) {
    methods::slot(x, slotname)
  } else {
    x[[slotname]]
  }
}

git2r_workdir <- function(x) {
  if (isS4(x)) {
    # Remove trailing slash
    stringr::str_replace(git2r::workdir(x), "/$", "")
  } else {
    git2r::workdir(x)
  }
}

# Cover all edge cases by passing original paths, absolute paths, and paths that
# are relative to root of Git repo.
git2r_add <- function(r, files, force = FALSE) {

  # Confirm that Git repository isn't locked
  check_git_lock(r)

  git2r::add(r, files, force = force)
  git2r::add(r, absolute(files), force = force)
  git2r::add(r, relative(files, start = git2r_workdir(r)), force = force)
}
