#' Extract a commit from a Git repository
#'
#' \code{extract_commit} extracts the 7-digit SHA1 identifier and message for a
#' specified commit.
#'
#' @param path character. Specify the path to a directory that is a Git
#'   repository (or any subdirectory of the Git repository).
#' @param num numeric. The number of the commit to extract in reverse
#'   chronological order. In other words, 1 is the most recent commit, 2 is the
#'   second most recent commit, etc.
#'
#' @return A list with the named elements \code{sha1} and \code{message} (both
#'   characters). If a Git repository is not found at \code{path}, both are
#'   \code{NA}.
#'
#' @examples
#' \dontrun{
#' # Most recent commit
#' extract_commit(".", 1)
#' # Penultimate commit
#' extract_commit(".", 2)
#' }
#' @export
extract_commit <- function(path, num) {
  stopifnot(file.exists(path),
            is.numeric(num),
            num == trunc(num),
            num > 0)
  if (!git2r::in_repository(path)) {
    return(list(sha1 = "NA", message = "NA"))
  }
  repo <- git2r::repository(path, discover = TRUE)
  git_log <- utils::capture.output(git2r::reflog(repo))
  total_commits <- length(git_log)
  if (total_commits == 0) {
    return(list(sha1 = "NA", message = "NA"))
  }
  if (num > total_commits) {
    stop(sprintf("Invalid search: %d. This repo only has %d commits.",
                 num, total_commits))
  }
  commit <- git_log[num]
  sha1 <- substr(commit, 2, 8)
  commit_message <- strsplit(commit, split = "commit: ")[[1]][2]
  return(list(sha1 = sha1, message = commit_message))
}

# Create a default .gitignore file
#
# The .gitignore in inst/infrastrucure does not survive builing the R package.
# The .nojekyll does, so it must be specific to this filename and not a
# property of hidden files. Hadley does not include .gitignore in
# .Rbuildignore, which further supports that it is ignored by default.
create_gitignore <- function(path, overwrite = FALSE) {
  lines <- c(".Rproj.user",
             ".Rhistory",
             ".RData",
             ".Ruserdata",
             ".DS_Store",
             "analysis/figure",
             "analysis/*html",
             "analysis/*_cache")
  fname <- file.path(path, ".gitignore")
  exists <- file.exists(fname)
  if (exists & !overwrite) {
    warning(sprintf("File %s already exists. Set overwrite = TRUE to replace",
                    fname))
  } else {
    writeLines(lines, con = fname)
  }
  return(invisible(fname))
}
