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

# Obtain all the committed files in a Git repository at a given commit.
#
# The default is to use the head commit.
get_committed_files <- function(repo, commit = NULL) {
  if (is.null(commit)) {
    commit <- git2r::lookup(repo, git2r::branch_target(git2r::head(repo)))
  }
  tree <- git2r::tree(commit)
  files <- ls_files(tree)
  return(files)
}

# List all files in a given "git_tree" object.
ls_files <- function (tree) {
  tree_list <- methods::as(tree, "list")
  tree_df <- methods::as(tree, "data.frame")
  names(tree_list) <- tree_df$name
  files <- tree_df$name[tree_df$type == "blob"]
  dirs <- tree_df$name[tree_df$type == "tree"]
  out <- files
  # Recurisvely call ls_files on the "git_tree" objects corresponding to each
  # subdirectory
  for (dir in dirs) {
    tree_next <- tree_list[[dir]]
    out <- c(out, file.path(dir, ls_files(tree_next)))
  }
  return(out)
}

# Get the files that have been committed to the repsitory more recently than
# their corresponding HTML files.
#
# repo: git_repository object
# files: character vector of filenames
# outdir: directory with website files
get_outdated_files <- function(repo, files, outdir = NULL) {
  ext <- tools::file_ext(files)
  if (!all(grepl("[Rr]md", ext)))
    stop("Only R Markdown files are accepted.")
  # Corresponding HTML files
  html <- to_html(files, outdir = outdir)
  # Remove preceding path if necessary. Has to be relative to .git directory.
  path_to_git <- git2r::workdir(repo)
  files <- stringr::str_replace(files, path_to_git, "")
  html <- stringr::str_replace(html, path_to_git, "")
  # For each source file, determine if it has been committed more recently than
  # its corresponding HTML
  out_of_date <- logical(length = length(files))
  for (i in seq_along(files)) {
    # Most recent commit time of source and HTML files
    recent_source <- get_recent_commit_time(repo, files[i])
    recent_html <- get_recent_commit_time(repo, html[i])
    if (recent_source >= recent_html) {
      out_of_date[i] <- TRUE
    }
  }
  outdated <- files[out_of_date]
  # Prepend path to Git repository
  outdated <- paste0(path_to_git, outdated)
  return(outdated)
}

# Get the time of the most recent commit for a file.
#
# repo: git_repository object
# f: path to file relative to .git
#
# Note: This function is not vectorized.
get_recent_commit_time <- function(repo, f) {
  # Obtain every commit for the file
  blame <- git2r::blame(repo, f)
  # Extract the times of the commits
  times <- sapply(blame@hunks,
                  function(x) git2r::when(x@final_signature@when))
  times <- strptime(times, format = "%Y-%m-%d %H:%M:%S")
  times <- sort(unique(times), decreasing = TRUE)
  # Most recent commit time
  recent <- times[1]
  return(recent)
}
