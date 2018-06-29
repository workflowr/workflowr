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
    git2r::head(x)
  } else {
    git2r::repository_head(x)
  }
}

git2r_slot <- function(x, slotname) {
  if (isS4(x)) {
    methods::slot(x, slotname)
  } else {
    x[[slotname]]
  }
}

# Add back trailing slash
git2r_workdir <- function(x) {
  if (isS4(x)) {
    git2r::workdir(x)
  } else {
    paste0(git2r::workdir(x), "/")
  }
}
