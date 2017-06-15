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
             ".Rapp.history",
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
  n_commits <- length(git2r::commits(repo))
  if (n_commits == 0) {
    stop(wrap("The Git repository has no commits yet."))
  }
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

# Get the files that have been committed to the repository more recently than
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

# Decide which files to render and commit
#
# Recursively search the commit log until the R Markdown file or its
# corresponding HTML file is found. If the Rmd is found first, the HTML file
# needs to be re-rendered, added, and committed (return TRUE). If the HTML file
# is found first, then it is up-to-date (return FALSE).
#
# @seealso \code{\link{obtain_files_in_commit}},
#   \code{\link{obtain_files_in_commit_root}}, \code{\link{wflow_commit}}
decide_to_render <- function(repo, log, rmd) {
  stopifnot(class(repo) == "git_repository",
            class(log) == "list",
            is.character(rmd))
  if (length(log) == 0) {
    warning("File not found in commit log: ", rmd)
    return(NA)
  } else {
    stopifnot(sapply(log, function(x) class(x) == "git_commit"))
  }
  html <- file.path("docs", stringr::str_replace(basename(rmd), "Rmd$", "html"))
  # Obtain the files updated in the most recent commit, similar to `git
  # status --stat`
  parent_commit <- git2r::parents(log[[1]])
  # The next action depends on what kind of commit is the most recent. Skip
  # merge commits (2 parents). Obtain files from a standard commit (1 parent)
  # using obtain_files_in_commit. Obtain files from root commit (0 parents)
  # using obtain_files_in_commit_root.
  if (length(parent_commit) == 2) {
    return(decide_to_render(repo, log[-1], rmd))
  } else if (length(parent_commit) == 1) {
    files <- obtain_files_in_commit(repo, log[[1]])
  } else if (length(parent_commit) == 0) {
    files <- obtain_files_in_commit_root(repo, log[[1]])
  }
  # Decide if the R Markdown file should be rendered (it has been updated most
  # recently), not rendered (the HTML has been updated more recently), or to
  # continue searching the commit log (neither the Rmd nor HTML has been
  # observed in the commit log yet).
  if (rmd %in% files) {
    return(TRUE)
  } else if (html %in% files) {
    return(FALSE)
  } else {
    return(decide_to_render(repo, log[-1], rmd))
  }

  # This final return should only be executed if there is an error in the
  # recursive function.
  return(files)
}

# Obtain the files updated in a commit
#
# Obtain the files updated in a commit, similar to \code{git status --stat}, by
# running a diff between the trees pointed to by the commit and its parent
# commit.
#
# This only works for commits that have one parent commit. Thus it will fail
# for merge commits (two parents) or the initial root commit (zero parents).
# two most recent commits. This uses `diff,git_tree`. See the source code at
# \url{https://github.com/ropensci/git2r/blob/89d916f17cb979b3cc21cbb5834755a2cf075f5f/R/diff.r#L314}
# and examples at
# \url{https://github.com/ropensci/git2r/blob/cb30b1dd5f8b57978101ea7b7dc26ae2c9eed38e/tests/diff.R#L88}.
#
# @seealso \code{\link{obtain_files_in_commit_root}},
#   \code{\link{decide_to_render}}
obtain_files_in_commit <- function(repo, commit) {
  stopifnot(class(repo) == "git_repository",
            class(commit) == "git_commit")
  parent_commit <- git2r::parents(commit)
  if (length(parent_commit) != 1) {
    stop(sprintf("Cannot perform diff on commit %s because it has %d parents",
                 commit@sha, length(parent_commit)))
  }
  git_diff <- git2r::diff(git2r::tree(commit),
                          git2r::tree(parent_commit[[1]]))
  files <- sapply(git_diff@files, function(x) x@new_file)
  return(files)
}

# Obtain the files updated in the root commit
#
# The files included in the root commit cannot be determined comparing two
# trees (which is how \code{\link{obtain_files_in_commit}} works). See
# \href{http://stackoverflow.com/questions/41433034/how-to-obtain-files-included-in-initial-commit-using-git2r-libgit2}{this
# Stack Overflow question} for details.
#
# This only works for the root commit, i.e. it must have no parents.
#
# @seealso \code{\link{obtain_files_in_commit}}, \code{\link{decide_to_render}}
obtain_files_in_commit_root <- function(repo, commit) {
  # Obtain the files in the root commit of a Git repository
  stopifnot(class(repo) ==  "git_repository",
            class(commit) == "git_commit",
            length(git2r::parents(commit)) == 0)
  entries <- methods::as(git2r::tree(commit), "data.frame")
  files <- character()
  while (nrow(entries) > 0) {
    if (entries$type[1] == "blob") {
      # If the entry is a blob, i.e. file:
      #  - record the name of the file
      #  - remove the entry
      files <- c(files, entries$name[1])
      entries <- entries[-1, ]
    } else if (entries$type[1] == "tree") {
      # If the entry is a tree, i.e. subdirectory:
      #  - lookup the entries for this tree
      #  - add the subdirectory to the name so that path is correct
      #  - remove the entry from beginning and add new entries to end of
      #    data.frame
      new_tree_df <- methods::as(git2r::lookup(repo, entries$sha[1]), "data.frame")
      new_tree_df$name <- file.path(entries$name[1], new_tree_df$name)
      entries <- rbind(entries[-1, ], new_tree_df)
    } else {
      stop(sprintf("Unknown type %s found in commit %s",
                   entries$type[1], commit))
    }
  }

  return(files)
}
