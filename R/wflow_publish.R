
#'Publish the site
#'
#'\code{wflow_publish} is the main workflowr function. Use it when you are ready
#'to publish an analysis to your site. \code{wflow_publish} performs three
#'steps: 1) commit the file(s), 2) rebuild the file(s), 3) commit the generated
#'website file(s). These steps ensure that the version of the HTML file is
#'created by the latest version of the R Markdown file, which is critical for
#'reproducibility.
#'
#'@inheritParams wflow_commit_
#'@inheritParams wflow_build_
#'
#'@return Invisibly returns the full path to the R Markdown file(s).
#'
#'@seealso \code{\link{wflow_commit_}}, \code{\link{wflow_build_}}
#'
#' @examples
#' \dontrun{
#' # single file
#' wflow_publish("analysis/file.Rmd")
#' # All tracked files that have been updated
#' wflow_publish()
#' # A new file plus all tracked files that have been updated
#' wflow_publish("analysis/file.Rmd", all = TRUE)
#' # Multiple files
#' wflow_publish(c("analysis/file.Rmd", "analysis/another.Rmd"))
#' # All R Markdown files that start with the pattern "new_"
#' wflow_publish(Sys.glob("analysis/new_*Rmd"))
#'
#' }
#'
#'@import rmarkdown
#'
wflow_publish <- function(
  # args to wflow_commit
  files = NULL,
  message = NULL,
  all = is.null(files),
  force = FALSE,
  # args to wflow_build
  update = FALSE,
  everything = FALSE,
  # general
  dry_run = FALSE,
  path = "."
  ) {
  # To do:
  # * Warning for cache directories
  # * Warning if files in docs/ included
  # Check for modifications to _site.yml. Refuse to build if it is modified

  if (is.null(message)) {
    message <- sys.call()
  }

  # Commit the provided files
  f_committed <- wflow_commit_(files = files, message = message,
                               all = all, force = force,
                               dry_run = dry_run, path = path)
  # Build the site
  f_built <- wflow_build_(files = f_committed, make = FALSE,
                          update = update, everything = everything,
                          local = FALSE, dry_run = dry_run, path = path)
  # Commit the site
  f_committed_site <- wflow_commit_(files = f_built, message = "Build site.",
                                    all = FALSE, force = force,
                                    dry_run = dry_run, path = path)
  f_committed_all <- sort(c(f_committed, f_committed_site))
  return(invisible(f_committed_all))
}

#' Build the site
#'
#' \code{wflow_build} builds the website from the files in the analysis
#' directory. This is intended to be used when developing your code to preview
#' the changes. When you are ready to commit the files, use
#' \code{\link{wflow_publish}}.
#'
#' \code{wflow_build} has multiple, non-mutually exclusive options for deciding
#' which files to build:
#'
#' \itemize{
#'
#' \item Files specified via the argument \code{files} are always built.
#'
#' \item If \code{make = TRUE}, all files which have been modified more recently
#' than their corresponding HTML files will be built.
#'
#' \item If \code{update = TRUE}, all files which have been committed more
#' recently than their corresponding HTML files will be built. However, files
#' which currently have staged or unstaged changes will be ignored.
#'
#' \item If \code{everything = TRUE}, all files will be built.
#'
#' }
#'
#' Under the hood, \code{wflow_build} is a wrapper for
#' \code{\link[rmarkdown]{render_site}} from the package \link{rmarkdown}.
#'
#' @param files character (default: NULL). Files to build. Supports file
#'   extensions Rmd, rmd, and md. Only files in the analysis directory are
#'   allowed (and therefore any path to a file is ignored).
#' @param make logical (default: \code{is.null(files)}). Use Make-like behavior,
#'   i.e. build the files that have been updated more recently than their
#'   corresponding HTML files. This is the default action if no files are
#'   specified.
#' @param update logical (default: FALSE). Build the files that have been
#'   committed more recently than their corresponding HTML files (and do not
#'   have any unstaged or staged changes). This ensures that the commit version
#'   ID inserted into the HTML corresponds to the exact version of the source
#'   file that was used to produce it.
#' @param everything logical (default: FALSE). Build all R Markdown (and
#'   Markdown) files. Useful for site-wide changes like updating the theme,
#'   navigation bar, or any other setting in \code{_site.yml}.
#' @param seed numeric (default: 12345). The seed to set before building each
#'   file. Passed to \code{\link{set.seed}}.
#' @param log_dir character (default: NULL). The directory to save log files
#'   from building files. It will be created if necessary and ignored if
#'   \code{local = TRUE}. The default is to create a directory in \code{/tmp}.
#' @param local logical (default: FALSE). Build files locally in the R console.
#'   This should only be used for debugging purposes. The default is to build
#'   each file in its own separate fresh R process to ensure each file is
#'   reproducible in isolation.
#' @param dry_run logical (default: FALSE). Preview the files to be built, but
#'   do not actually build them.
#' @inheritParams wflow_commit_
#'
#' @return A character vector of the built files
#'
#' @seealso \code{\link{wflow_publish}}
#'
#' @examples
#' \dontrun{
#'
#' # Build all files
#' wflow_build() # equivalent to wflow_build(make = TRUE)
#' # Build a single file
#' wflow_build("file.Rmd")
#' # Build multiple files
#' wflow_build(c("file1.Rmd", "file2.Rmd"))
#' # Build every file
#' wflow_build(everything = TRUE)
#' }
#'
#' @import rmarkdown
#'
wflow_build_ <- function(files = NULL, make = is.null(files),
                         update = FALSE, everything = FALSE,
                         seed = 12345, log_dir = NULL,
                         local = FALSE, dry_run = FALSE, path = ".") {
  if (!(is.null(files) | is.character(files)))
    stop("files must be NULL or a character vector")
  if (!is.logical(make) | length(make) != 1)
    stop("make must be a one-element logical vector")
  if (!is.logical(update) | length(update) != 1)
    stop("update must be a one-element logical vector")
  if (!is.logical(everything) | length(everything) != 1)
    stop("everything must be a one-element logical vector")
  if (!is.numeric(seed) | length(seed) != 1)
    stop("seed must be a one element numeric vector")
  if (!(is.null(log_dir) | (is.character(log_dir) & length(log_dir) == 1)))
    stop("log_dir must be a one element character vector")
  if (!is.logical(local) | length(local) != 1)
    stop("local must be a one-element logical vector")
  if (!is.logical(dry_run) | length(dry_run) != 1)
    stop("dry_run must be a one-element logical vector")
  if (!is.character(path) | length(path) != 1)
    stop("path must be a one element character vector")

  # Check that directories and files exist
  if (!dir.exists(path)) {
    stop("path does not exist.")
  } else {
    path <- normalizePath(path)
  }
  if (is.null(log_dir))
    log_dir <- "/tmp/workflowr"
  if (!is.null(files)) {
    files_missing <- !file.exists(files)
    if (any(files_missing)) {
      stop("missing files: ", files[files_missing])
    } else {
      files <- normalizePath(files)
    }
  }
  files_to_build <- files

  root_path <- rprojroot::find_rstudio_root_file(path = path)
  analysis_dir <- file.path(root_path, "analysis")

  files_all <- Sys.glob(file.path(analysis_dir, "*Rmd"))

  if (make) {
    files_make <- return_modified_rmd(files_all)
    files_to_build <- union(files_to_build, files_make)
  }

  # This currently gets every Rmd file. May want to change to only tracked files
  if (everything) {
    files_to_build <- files_all
  } else if (update) {
    # Build files if their corresponding HTML file in out-of-date in the Git
    # commit history
    #
    # To do: Adapt from wflow_commit
  }

  if (!dry_run) {
    for (f in files_to_build) {
      cat(sprintf("\n\nRendering %s\n\n", f))
      if (local) {
        build_rmd(f, seed = seed, envir = new.env())
      } else {
        build_rmd_external(f, seed = seed, log_dir = log_dir)
      }
    }
  }

  return(invisible(files_to_build))
}


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
#' @param path character (default: ".") By default the function assumes the
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
                          force = FALSE, dry_run = FALSE, path = ".") {
  if (!(is.null(files) | is.character(files)))
    stop("files must be NULL or a character vector")
  if (!(is.null(message) | is.character(message)))
    stop("files must be NULL or a character vector")
  if (!is.logical(all) | length(all) != 1)
    stop("all must be a one-element logical vector")
  if (!is.logical(dry_run) | length(dry_run) != 1)
    stop("dry_run must be a one-element logical vector")
  if (!is.character(path) | length(path) != 1)
    stop("path must be a one element character vector")
  if (!dir.exists(path))
    stop("path does not exist.")

  if (is.null(files) && !all)
    stop("Must specify files to commit, set `all = TRUE`, or both",
         call. = FALSE)

  # Establish connection to Git repository
  path <- normalizePath(path)
  root_path <- try(rprojroot::find_rstudio_root_file(path = path))
  if (class(root_path) == "try-error")
    stop("Unable to find RStudio Rproj file ",
         "at the root of the workflowr project",
         call. = FALSE)
  r <- try(git2r::repository(root_path, discover = TRUE))
  if (class(r) == "try-error")
    stop("Unable to locate Git repository in ", path, call. = FALSE)
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
