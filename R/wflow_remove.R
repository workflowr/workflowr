#' Remove files
#'
#' \code{wflow_remove} removes files. If the file to be removed is an R Markdown
#' file, the corresponding HTML and other related files are also removed. If the
#' workflowr project uses Git, \code{wflow_remove} commits the changes.
#'
#' @param files character. Files to be removed. Supports file
#'   \href{https://en.wikipedia.org/wiki/Glob_(programming)}{globbing}.
#' @param git logical (default: TRUE). Commit the changes (only applicable if
#'   Git repository is present).
#' @param dry_run logical (default: FALSE). Preview the files to be removed but
#'   do not actually remove them.
#' @inheritParams wflow_git_commit
#'
#' @return An object of class \code{wflow_remove}, which is a list with the
#'   following elements:
#'
#'   \itemize{
#'
#'   \item \bold{files}: The relative path(s) to the removed file(s).
#'
#'   \item \bold{message}: The message describing the commit (if applicable).
#'
#'   \item \bold{dry_run}: The input argument \code{dry_run}.
#'
#'   \item \bold{commit}:The object returned by
#'   \link{git2r}::\code{\link[git2r]{commit}} (only included if \code{dry_run
#'   == FALSE}).
#'
#'   \item \bold{files_git}: The relative path(s) to the file(s) removed from
#'   the Git repository.
#'
#'   }
#'
#' @seealso \code{\link{wflow_git_commit}}
#'
#' @examples
#' \dontrun{
#'
#' # Remove a single file
#' wflow_remove("analysis/file.Rmd", "Remove old analysis.")
#' # Remove multiple files
#' wflow_remove(c("analysis/file.Rmd", "output/small-data.txt"),
#'              "Remove old analysis and its associated data.")
#' }
#'
#' @export
wflow_remove <- function(files,
                         message = NULL,
                         git = TRUE,
                         dry_run = FALSE,
                         project = ".") {

  # Check input arguments ------------------------------------------------------

  if (!(is.character(files) && length(files) > 0))
    stop("files must be a character vector of filenames")
  files <- glob(files)
  if (!all(fs::file_exists(files) | fs::dir_exists(files)))
    stop("Not all files exist. Check the paths to the files")
  # Change filepaths to relative paths
  files <- relative(files)

  if (is.null(message)) {
    message <- deparse(sys.call())
    message <- paste(message, collapse = "\n")
  } else if (is.character(message)) {
    message <- wrap(paste(message, collapse = " "))
  } else {
    stop("message must be NULL or a character vector")
  }

  if (!(is.logical(git) && length(git) == 1))
    stop("git must be a one-element logical vector")

  if (!(is.logical(dry_run) && length(dry_run) == 1))
    stop("dry_run must be a one-element logical vector")

  if (!(is.character(project) && length(project) == 1))
    stop("project must be a one-element character vector")

  check_wd_exists()

  if (!fs::dir_exists(project)) {
    stop("project directory does not exist.")
  }

  project <- absolute(project)

  # Assess project status ------------------------------------------------------

  p <- wflow_paths(project = project)

  # Should changes be committed?
  if (!is.na(p$git) && git) {
    use_git <- TRUE
    r <- git2r::repository(path = p$git)
  } else {
    use_git <- FALSE
  }

  if (use_git && !dry_run) check_git_config(project,
                                            "`wflow_remove` with `git = TRUE`")

  # Gather files to remove -----------------------------------------------------

  # Are any of the specified files R Markdown files in the analysis directory?
  files_ext <- tools::file_ext(files)
  files_rmd <- files[files_ext %in% c("Rmd", "rmd")]
  files_rmd <- files_rmd[absolute(files_rmd) ==
                         absolute(file.path(p$analysis, basename(files_rmd)))]

  # If the user inputs a directory, obtain all the files in those directories so
  # that they can be removed from the Git repo if applicable.
  is_dir <- fs::dir_exists(files)
  files_to_remove <- files[!is_dir]
  dirs_to_remove <- files[is_dir]
  for (d in dirs_to_remove) {
    d_files <- list.files(path = d, full.names = TRUE)
    files_to_remove <- c(files_to_remove, d_files)
  }

  for (rmd in files_rmd) {
    # Corresponding HTML?
    html <- to_html(rmd, outdir = p$docs)
    if (fs::file_exists(html)) {
      files_to_remove <- c(files_to_remove, html)
    }
    # Any figure files in analysis directory?
    fig_path <- create_figure_path(rmd)
    if (p$analysis == ".") {
      dir_figs_analysis <- fig_path
    } else {
      dir_figs_analysis <- file.path(p$analysis, fig_path)
    }
    figs_analysis <- list.files(path = dir_figs_analysis, full.names = TRUE)
    if (length(figs_analysis) > 0) {
      files_to_remove <- c(files_to_remove, figs_analysis)
      dirs_to_remove <- c(dirs_to_remove, dir_figs_analysis)
    }
    # Any figure files in docs directory?
    if (p$docs == ".") {
      dir_figs_docs <- fig_path
    } else {
      dir_figs_docs <- file.path(p$docs, fig_path)
    }
    figs_docs <- list.files(path = dir_figs_docs, full.names = TRUE)
    if (length(figs_docs) > 0) {
      files_to_remove <- c(files_to_remove, figs_docs)
      dirs_to_remove <- c(dirs_to_remove, dir_figs_docs)
    }
    # Cache directory?
    dir_cache <- paste0(tools::file_path_sans_ext(basename(rmd)), "_cache")
    if (p$analysis != ".") {
      dir_cache <- file.path(p$analysis, dir_cache)
    }
    if (fs::dir_exists(dir_cache)) {
      files_cache <- list.files(path = dir_cache, full.names = TRUE,
                                recursive = TRUE)
      files_to_remove <- c(files_to_remove, files_cache)
      dirs_to_remove <- c(dirs_to_remove, dir_cache)
    }
  }

  # Gather files to remove from Git --------------------------------------------

  if (use_git) {
    # Obtain committed files
    files_committed <- relative(get_committed_files(r))

    # Obtain files to be removed from Git
    logical_files_git <- files_to_remove %in% files_committed
    files_to_remove_from_git <- files_to_remove[logical_files_git]
  } else {
    files_to_remove_from_git <- NA
  }

  # Remove files ---------------------------------------------------------------

  if (!dry_run) {
    wflow_delete(files_to_remove)
    # Remove the empty (though potentially nested) directories
    unlink(dirs_to_remove, recursive = TRUE)
  }

  # Commit removed files -------------------------------------------------------

  if (use_git && !dry_run && length(files_to_remove_from_git) > 0) {
    git2r_add(r, files_to_remove_from_git)
    git2r::commit(r, message = message)
    commit <- git2r::commits(r, n = 1)[[1]]
  } else {
    commit <- NA
  }

  # Prepare output -------------------------------------------------------------

  o <- list(files = files_to_remove,
            message = message,
            dry_run = dry_run,
            commit = commit,
            files_git = files_to_remove_from_git)
  class(o) <- "wflow_remove"
  return(o)
}

#' @export
print.wflow_remove <- function(x, ...) {
  cat("Summary from wflow_remove\n\n")
  if (x$dry_run) {
    cat(wrap("The following would be removed:"), "\n\n")
  } else {
    cat(wrap("The following was removed:"), "\n\n")
  }
  cat(x$files, sep = "\n")

  if (length(x$files_git) > 0 && !all(is.na(x$files_git))) {
    if (x$dry_run) {
      cat("\n", wrap("The following would be removed from the Git repo:"),
          "\n\n", sep = "")
    } else {
      cat("\n", wrap(sprintf(
        "The following was removed from the Git repo in commit %s:",
        stringr::str_sub(x$commit$sha, start = 1, end = 7))),
        "\n\n", sep = "")
    }
    cat(x$files_git, sep = "\n")
    cat("\ncommit message:\n")
    cat(x$message)
    cat("\n")
  }

  return(invisible(x))
}
