# Utility functions to wrap git2r

# Wrapper for git2r::merge.git_repository().
#
# Needed because the returned values for class git_merge_result are inconsistent.
#
# https://github.com/ropensci/git2r/pull/321
# https://github.com/ropensci/git2r/issues/389
# https://github.com/ropensci/git2r/pull/391
#
# x - git_repository object
# b - character string of existing branch name
# fail - Passed to merge. From docs:
#      > If a conflict occurs, exit immediately instead of attempting to
#      continue resolving conflicts. Default is FALSE.
#
# See ?git2r::merge.git_repository for more details.
git2r_merge <- function(x, b, fail = FALSE) {
  stopifnot(inherits(x, "git_repository"))
  stopifnot(is.character(b))
  stopifnot(is.logical(fail), length(fail) == 1)

  m <- base::merge(x, b, fail = fail)
  if (length(m$fast_forward) == 0) m$fast_forward <- FALSE
  if (length(m$conflicts) == 0) m$conflicts <- FALSE
  if (length(m$sha) == 0) m$sha <- NA_character_
  return(m)
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
