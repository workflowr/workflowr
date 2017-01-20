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

#' Commit the website files
#'
#' \code{wflow_commit} builds and commits the website files, ensuring that the
#' website files are created by the proper R Markdown files that have been
#' committed to the Git repository. Optionally can specify files to be committed
#' before building the website.
#'
#' worklowr facilitates reproducibility by placing the current SHA-1 of the Git
#' repository at the top of each HTML file. This indicates which version of the
#' code could be used to reproduce the results. In order for this to be
#' meaningful, the R Markdown file must not have changed since it was last
#' committed. \code{wflow_commit} can be invoked in 3 different (though not
#' mutually exclusive) modes.
#'
#' First, running \code{wflow_commit} with the default arguments will identify
#' all R Markdown files which have been modified more recently in the Git commit
#' history than their corresponding HTML files. Furthermore these files must not
#' currently have any subsequent changes that have not been committed. The files
#' will be built and the correspoding HTML committed.
#'
#' Second, you can have \code{wflow_commit} first add and commit files specified
#' with the argument \code{commit_files}. A message for this commit can also be
#' specified with the argument \code{commit_message}. After this commit has been
#' made, \code{wflow_commit} then searches the Git commit history as described
#' above.
#'
#' Third, you can have \code{wflow_commit} re-build and commit all the webpages
#' by setting \code{all = TRUE}. This is useful if you are making an aesthetic
#' change, e.g. the theme, that needs to be applied regardless of whether the R
#' Markdown file has been edited. Only tracked files without uncommitted changes
#' will be re-built (this prevents the HTML not matching the corresponding R
#' Markdown file).
#'
#' @param all logical indicating if every R Markdown file should be rendered
#'   when building and committing the site (default: FALSE).
#' @param commit_files Files to be committed to Git before building and
#'   committing website files (default: NULL).
#' @param commit_message A commit message. Only used if specific files are
#'   specified to the argument \code{files} (default: NULL).
#' @param dry_run Identifies R Markdown files that have been updated, but does
#'   not render them.
#' @param include_staged logical. By default \code{wflow_commit} will stop if it
#'   detects any files in the staging area. Set to TRUE ff you want these files
#'   to be included in the commit created by \code{wflow_commit} (not
#'   recommended).
#' @param path By default the function assumes the current working directory is
#'   within the project. If this is not true, you'll need to provide the path to
#'   the project directory.
#'
#' @return If \code{dry_run = TRUE}, returns the character vector of R Markdown
#'   files that would be rendered. Otherwise invisibly returns this vector.
#'
#' @examples
#' \dontrun{
#' # Build and commit the webpages that are out of date
#' wflow_commit()
#' # Specify files to commit (with a corresponding commit message),
#' # prior to building and committing the webpages
#' wflow_commit(commit_files = c("pipeline.R", "new-analysis.Rmd"),
#'              commit_message = "Finished new analysis")
#' # Re-build all the webpages
#' # (e.g. to implement an aesthetic change)
#' wflow_commit(all = TRUE)
#' }
#' @import rmarkdown
#' @export
wflow_commit <- function(all = FALSE, commit_files = NULL,
                         commit_message = NULL, dry_run = FALSE,
                         include_staged = FALSE, path = ".") {
  stopifnot(is.logical(all),
            is.null(commit_files) | is.character(commit_files),
            is.null(commit_message) | is.character(commit_message),
            is.logical(dry_run),
            is.character(path))
  root_path <- rprojroot::find_rstudio_root_file(path = path)
  analysis_dir <- file.path(root_path, "analysis")
  stopifnot(dir.exists(analysis_dir))
  repo <- git2r::repository(root_path)
  s <- git2r::status(repo)
  num_staged <- length(s$staged)

  if (num_staged > 0) {
    warning("Files have already been added to the staging area.")
    warning("You probably want to commit them first before running commit_site")
  }
  if (num_staged > 0 & !include_staged) {
    stop("wflow_commit stopped because of files in the staging area. Either commit these first or set the argument `include_staged = TRUE` to include these files in the commit created by wflow_commit.")
  }

  if (!is.null(commit_files)) {
    stopifnot(file.exists(commit_files))
    if (dry_run) {
      message("The current status of the Git repo is:")
      message(paste(utils::capture.output(s), collapse = "\n"))
      message("You are planning to commit the following files before building the site:")
      message(paste(commit_files, collapse = "\n"))
    } else {
      git2r::add(repo, commit_files)
      s <- git2r::status(repo)
      num_staged <- length(s$staged)
      if (num_staged == 0) {
        warning("None of the commit_files provided were committed, presumably because they have not been updated.")
      } else if (is.null(commit_message)) {
        git2r::commit(repo, message = "Files commited by wflow_commit.")
      } else{
        git2r::commit(repo, message = commit_message)
      }
    }
  }

  # Gather Rmd files
  rmd_all <- list.files(path = analysis_dir, pattern = "^[^_].*Rmd$")
  rmd_all <- file.path("analysis", rmd_all)

  # Remove from consideration any R Markdown files that
  #  1. Have changes in the working directory or staging area
  #  2. Are untracked
  #  3. Are ignored by .gitignore
  # Determined by running `git status`
  git_status <- git2r::status(repo, ignored = TRUE)
  staged_files <- unlist(git_status$staged)
  staged_rmd <- staged_files[grep("Rmd$", staged_files)]
  unstaged_files <- unlist(git_status$unstaged)
  unstaged_rmd <- unstaged_files[grep("Rmd$", unstaged_files)]
  untracked_files <- unlist(git_status$untracked)
  untracked_rmd <- untracked_files[grep("Rmd$", untracked_files)]
  ignored_files <- unlist(git_status$ignored)
  ignored_rmd <- ignored_files[grep("Rmd$", ignored_files)]

  rmd_to_consider <- setdiff(rmd_all, c(staged_rmd, unstaged_rmd,
                                        untracked_rmd, ignored_rmd))
  # If all eligible R Markdown files should be built
  if (all) {
    to_render <- TRUE
  } else {
    # Determine which R Markdown files need to be updated
    to_render <- logical(length = length(rmd_to_consider))
    log <- git2r::commits(repo)
    for (i in seq_along(rmd_to_consider)) {
      to_render[i] <- decide_to_render(repo, log, rmd_to_consider[i])
    }
  }

  files_to_update <- rmd_to_consider[to_render]
  files_to_update <- file.path(root_path, files_to_update)

  # Render the updated R Markdown files
  if (length(files_to_update) == 0) {
    message("Everything up-to-date")
  } else if (dry_run) {
    message("The HTML files would be built and comitted from the following R Markdown files:")
    return(files_to_update)
  } else {
    for (f in files_to_update) {
      # Delete the figures first? In both analysis/ and docs/?
      message(sprintf("\n\nRendering %s\n\n", basename(f)))
      rmarkdown::render_site(f, envir = new.env(), quiet = TRUE)
      html <- file.path(root_path, "docs",
                        stringr::str_replace(basename(f), "Rmd$", "html"))
      git2r::add(repo, html)
      figure <- file.path(root_path, "docs", "figure", basename(f))
      git2r::add(repo, figure)
    }
    site_libs <- file.path(root_path, "docs", "site_libs")
    nojekyll <- file.path(root_path, "docs", ".nojekyll")
    git2r::add(repo, site_libs)
    git2r::add(repo, nojekyll)
    git2r::commit(repo, message = "Build site.")
  }

  return(invisible(files_to_update))
}
