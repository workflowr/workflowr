#' Decide with files to render and commit
#'
#' Recursively search the commit log until the R Markdown file or its
#' corresponding HTML file is found. If the Rmd is found first, the HTML file
#' needs to be re-rendered, added, and committed (return TRUE). If the HTML file
#' is found first, then it is up-to-date (return FALSE).
#'
#' To do: What if the Rmd was added in the first commit?
decide_to_render <- function(repo, log, rmd) {
  stopifnot(class(repo) == "git_repository",
            class(log) == "list",
            sapply(log, function(x) class(x) == "git_commit"),
            is.character(rmd))
  if (length(log) == 1) {
    warning("File not found in commit log: ", rmd)
    return(NA)
  }
  html <- file.path("docs", stringr::str_replace(basename(rmd), "Rmd$", "html"))
  # Obtain the files updated in the most recent commit, similar to `git
  # status --stat`
  files <- obtain_files_in_commit(repo, log[[1]])
  if (rmd %in% files) {
    return(TRUE)
  } else if (html %in% files) {
    return(FALSE)
  } else {
    return(decide_to_render(repo, log[-1], rmd))
  }

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

#' Commit the website files
#'
#' \code{commit_site} ensures that the website files are created by the code
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
#' commit_site()
#' }
#' @export
commit_site <- function(dry_run = FALSE, path = ".") {
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

  if (length(files_to_update) == 0) {
    message("Everything up-to-date")
  }

  # Render the updated R Markdown files
  if (dry_run) {
    return(files_to_update)
  } else {
    for (f in files_to_update) {
      # Delete the figures first? In both analysis/ and docs/?
      cat(sprintf("\n\nRendering %s\n\n", f))
      rmarkdown::render_site(file.path(root_path, f))
      html <- file.path(root_path, "docs",
                        stringr::str_replace(basename(f), "Rmd$", "html"))
      git2r::add(repo, html)
      figure <- file.path(root_path, "docs", "figure", basename(f))
      git2r::add(repo, figure)
    }
    site_libs <- file.path(root_path, "docs", "site_libs")
    git2r::add(repo, site_libs)
    git2r::commit(repo, message = "Build site.")
  }

  return(invisible(files_to_update))
}
