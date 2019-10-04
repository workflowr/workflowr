# Utility functions to wrap git2r

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

# Cover all edge cases by passing original paths, absolute paths, and paths that
# are relative to root of Git repo.
git2r_add <- function(r, files, force = FALSE) {

  # Confirm that Git repository isn't locked
  check_git_lock(r)

  git2r::add(r, files, force = force)
  git2r::add(r, absolute(files), force = force)
  git2r::add(r, relative(files, start = git2r::workdir(r)), force = force)
}
