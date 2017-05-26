#' Commit files
#'
#' \code{wflow_commit} adds and commits files with Git. This is a convenience
#' function to run Git commands from the R console instead of the shell. For
#' most use cases, you should use \code{\link{wflow_publish}} instead, which
#' calls \code{wflow_commit} and then subsequently also builds and commits the
#' website files.
#'
#' Some potential use cases for \code{wflow_commit}:
#'
#' \itemize{
#'
#' \item Commit drafts which you do not yet want to be included in the website
#'
#' \item Commit files which do not directly affect the website (e.g. when you
#' are writing scripts for a data processing pipeline)
#'
#' \item Manually commit files in \code{docs/} (proceed with caution!). This
#' should only be done for content that is not automatically generated from the
#' source files in the analysis directory, e.g. an image file you want to
#' include in one of your pages.
#'
#' }
#'
#' Under the hood, \code{wflow_commit} is a wrapper for \code{\link[git2r]{add}}
#' and \code{\link[git2r]{commit}} from the package \link{git2r}.
#'
#' @param files character (default: NULL). Files to be added and committed with
#'   Git.
#' @param message character (default: NULL). A commit message.
#' @param all logical (default: FALSE). Automatically stage files that have been
#'   modified and deleted. Equivalent to: \code{git commit -a}
#' @param force logical (default: FALSE). Allow adding otherwise ignored files.
#'   Equivalent to: \code{git add -f}
#' @param dry_run logical (default: FALSE). Preview the proposed action but do
#'   not actually add or commit any files.
#' @param project character (default: ".") By default the function assumes the
#'   current working directory is within the project. If this is not true,
#'   you'll need to provide the path to the project directory.
#'
#' @return A character vector of the committed files
#'
#' @seealso \code{\link{wflow_publish}}
#'
#' @examples
#' \dontrun{
#'
#' # Commit a single file
#' wflow_commit("analysis/file.Rmd", "Add new analysis")
#' # Commit multiple files
#' wflow_commit(c("code/process-data.sh", "output/small-data.txt"),
#'              "Process data set")
#' # Add and commit all tracked files, similar to `git commit -a`
#' wflow_commit(message = "Lots of changes", all = TRUE)
#' }
#'
wflow_commit_ <- function(files = NULL, message = NULL, all = FALSE,
                          force = FALSE, dry_run = FALSE, project = ".") {
  if (!(is.null(files) | is.character(files)))
    stop("files must be NULL or a character vector")
  if (!(is.null(message) | is.character(message)))
    stop("message must be NULL or a character vector")
  if (!is.logical(all) | length(all) != 1)
    stop("all must be a one-element logical vector")
  if (!is.logical(dry_run) | length(dry_run) != 1)
    stop("dry_run must be a one-element logical vector")
  if (!is.character(project) | length(project) != 1)
    stop("project must be a one element character vector")
  if (!dir.exists(project))
    stop("project does not exist.")

  if (is.null(files) && !all)
    stop("Must specify files to commit, set `all = TRUE`, or both",
         call. = FALSE)

  # Establish connection to Git repository
  project <- normalizePath(project)
  root_path <- try(rprojroot::find_rstudio_root_file(path = project))
  if (class(root_path) == "try-error")
    stop("Unable to find RStudio Rproj file ",
         "at the root of the workflowr project",
         call. = FALSE)
  r <- try(git2r::repository(root_path, discover = TRUE))
  if (class(r) == "try-error")
    stop("Unable to locate Git repository in ", project, call. = FALSE)
  if (root_path != dirname(r@path))
    warning("The Git repository is not at the root of the workflowr project",
            "\nworkflowr project: ", root_path,
            "\nGit repo: ", dirname(r@path),
            call. = FALSE)
  status <- git2r::status(r, ignored = TRUE)

  # Collate files to be added and committed
  if (!is.null(files)) {
    # Identify non-existent files
    files_exist <- file.exists(files)
    if (length(files) == sum(!files_exist)) {
      stop("None of the files could be found. ",
           "Check your working directory and file paths.",
           call. = FALSE)
    } else if (any(!files_exist)) {
      warning("Non-existent files found.",
              "\nThe following files do not exist:\n\n",
              paste(files[!files_exist], collapse = "\n"),
              call. = FALSE)
      files <- files[files_exist]
    }
    files <- normalizePath(files)
    # Identify files outside the Git repository
    files_outside <- !stringr::str_detect(files, root_path)
    if (length(files) == sum(files_outside)) {
      stop("All of the files are outside of the Git repository. ",
           "Check your working directory and file paths.",
           call. = FALSE)
    } else if (any(files_outside)) {
      warning("Some files are outside of the Git repository ",
              "and therefore will **not** be included in the commit.",
              "\nThe following files are outside the Git repository:\n\n",
              paste(files[files_outside], collapse = "\n"),
              call. = FALSE)
      files <- files[!files_outside]
    }
    # Refuse to commit any untracked file larger than 100MB.
    files_untracked_all <- c(unlist(status$untracked),
                             unlist(status$ignored))
    files_untracked_all <- normalizePath(files_untracked_all)
    is_untracked <- files %in% files_untracked_all
    sizes <- file.size(files) / 10^6
    ok_size <- sizes < 100
    files_too_large <- is_untracked & !ok_size
    if (length(files) == sum(files_too_large)) {
      stop("Files must be less than 100 MB to push to GitHub. ",
           "None of the files meet this requirement. ",
           "Run Git from the commandline if you ",
           "really want to commit these files.",
           call. = FALSE)
    } else if (any(files_too_large)) {
      warning("Some files are larger than GitHub's limit of 100 MB ",
              "and therefore will **not** be included in the commit. ",
              "Run Git from the commandline if you ",
              "really want to commit these files.",
              "\nThe following files are larger than 100 MB:\n\n",
              paste(files[files_too_large], collapse = "\n"),
              call. = FALSE)
      files <- files[!files_too_large]
    }
  }

  # Send warning if staged files are present but not specified to be committed
  files_staged <- unlist(status$staged)
  if (length(files_staged) != 0) {
    files_staged_unexpected <- setdiff(normalizePath(files_staged), files)
    if (length(files_staged_unexpected) != 0)
      warning("There are unexpected staged changes ",
              "that will be included in commit.",
              "\nSpecifically the following files have staged changes:\n\n",
              paste(normalizePath(files_staged_unexpected), collapse = "\n"),
              call. = FALSE)
  }

  # Prepare commit message
  if (is.null(message)) {
    warning("It is strongly recommended to include a custom commit message.",
            call. = FALSE)
    message <- deparse(sys.call())
  } else {
    message <- paste(message, collapse = "\n")
  }

  # Send message for dry run, otherwise add/commit files
  if (dry_run) {
    message("Dry run settings",
            "\n----",
            "\n\n* Specified files to add:\n\n",
            if (is.null(files)) "NA" else paste(files, collapse = "\n"),
            "\n\n* message:\n\n", message,
            "\n\n* Automatically stage files that have been ",
            "modified and deleted (all): ", all,
            "\n\n* Allow adding otherwise ignored files (force): ", force,
            "\n\n")
  } else {
    # Add the specified files
    if (!is.null(files)) {
      git2r::add(r, files, force = force)
      # Send warning if no files were staged
      status_post_add <- git2r::status(r, ignored = TRUE)
      files_staged_post_add <- unlist(status_post_add$staged)
      files_added <- setdiff(files_staged_post_add, files_staged)
      if (length(files_added) == 0)
        warning("None of the specified files were added to the staging area. ",
                "Perhaps they have not been updated ",
                "or the changes had already been added.",
                call. = FALSE)
    }
    # Commit
    tryCatch(git2r::commit(r, message = message, all = all),
             error = function(e) {
               if (stringr::str_detect(e$message, "Nothing added to commit")) {
                 reason <- "Commit failed because no files were added."
               } else {
                 reason <- "Commit failed for unknown reason."
               }
               stop(reason, " Any untracked files must manually specified",
                    " even if `all = TRUE`.\n\n", call. = FALSE)
             }
    )
  }

  return(invisible(files))
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
