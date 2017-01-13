#' Decide with files to render and commit
#'
#' Recursively search the commit log until the R Markdown file or its
#' corresponding HTML file is found. If the Rmd is found first, the HTML file
#' needs to be re-rendered, added, and committed (return TRUE). If the HTML file
#' is found first, then it is up-to-date (return FALSE).
#'
#' @seealso \code{\link{obtain_files_in_commit}},
#'   \code{\link{obtain_files_in_commit_root}}, \code{\link{wflow_commit}}
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

#' Obtain the files updated in a commit
#'
#' Obtain the files updated in a commit, similar to \code{git status --stat}, by
#' running a diff between the trees pointed to by the commit and its parent
#' commit.
#'
#' This only works for commits that have one parent commit. Thus it will fail
#' for merge commits (two parents) or the initial root commit (zero parents).
#' two most recent commits. This uses `diff,git_tree`. See the source code at
#' \url{https://github.com/ropensci/git2r/blob/89d916f17cb979b3cc21cbb5834755a2cf075f5f/R/diff.r#L314}
#' and examples at
#' \url{https://github.com/ropensci/git2r/blob/cb30b1dd5f8b57978101ea7b7dc26ae2c9eed38e/tests/diff.R#L88}.
#'
#' @seealso \code{\link{obtain_files_in_commit_root}},
#'   \code{\link{decide_to_render}}
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

#' Obtain the files updated in the root commit
#'
#' The files included in the root commit cannot be determined comparing two
#' trees (which is how \code{\link{obtain_files_in_commit}} works). See
#' \href{http://stackoverflow.com/questions/41433034/how-to-obtain-files-included-in-initial-commit-using-git2r-libgit2}{this
#' Stack Overflow question} for details.
#'
#' This only works for the root commit, i.e. it must have no parents.
#'
#' @seealso \code{\link{obtain_files_in_commit}}, \code{\link{decide_to_render}}
obtain_files_in_commit_root <- function(repo, commit) {
  # Obtain the files in the root commit of a Git repository
  stopifnot(class(repo) ==  "git_repository",
            class(commit) == "git_commit",
            length(git2r::parents(commit)) == 0)
  entries <- as(git2r::tree(commit), "data.frame")
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
      new_tree_df <- as(git2r::lookup(repo, entries$sha[1]), "data.frame")
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
#' \code{wflow_commit} ensures that the website files are created by the code
#' that has been committed to the Git repository.
#'
#' @param dry_run Identifies R Markdown files that have been updated, but does
#'   not render them.
#' @param path By default the function assumes the current working directory is
#'   within the project. If this is not true, you'll need to provide the path to
#'   the project directory.
#'
#' @return If \code{dry_run = TRUE}, returns the character vector of R Markdown
#'   files that would be rendered. Otherwise invisibly returns this vector.
#'
#' @examples
#' \dontrun{
#' wflow_commit()
#' }
#' @export
wflow_commit <- function(dry_run = FALSE, path = ".") {
  root_path <- rprojroot::find_rstudio_root_file(path = path)
  analysis_dir <- file.path(root_path, "analysis")
  stopifnot(dir.exists(analysis_dir))

  # Gather Rmd files
  rmd_all <- list.files(path = analysis_dir, pattern = "Rmd$")
  rmd_all <- file.path("analysis", rmd_all)

  # Run `git status`
  repo <- git2r::repository(root_path)
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
  to_render <- logical(length = length(rmd_to_consider))
  log <- git2r::commits(repo)
  for (i in seq_along(rmd_to_consider)) {
    to_render[i] <- decide_to_render(repo, log, rmd_to_consider[i])
  }
  files_to_update <- rmd_to_consider[to_render]
  files_to_update <- file.path(root_path, files_to_update)

  if (length(files_to_update) == 0) {
    message("Everything up-to-date")
    return(invisible(files_to_update))
  }

  # Render the updated R Markdown files
  if (dry_run) {
    return(files_to_update)
  } else {
    for (f in files_to_update) {
      # Delete the figures first? In both analysis/ and docs/?
      cat(sprintf("\n\nRendering %s\n\n", basename(f)))
      rmarkdown::render_site(f)
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
