#' Publish the site
#'
#' \code{wflow_publish} is the main workflowr function. Use it when you are
#' ready to publish an analysis to your site. \code{wflow_publish} performs
#' three steps: 1) commit the file(s), 2) rebuild the file(s), 3) commit the
#' generated website file(s). These steps ensure that the version of the HTML
#' file is created by the latest version of the R Markdown file, which is
#' critical for reproducibility.
#'
#' @inheritParams wflow_commit_
#' @inheritParams wflow_build_
#'
#' @return Invisibly returns the full path to the R Markdown file(s).
#'
#' @seealso \code{\link{wflow_commit_}}, \code{\link{wflow_build_}}
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
#' @import rmarkdown
#'
wflow_publish <- function(
  # args to wflow_commit
  files = NULL,
  message = NULL,
  all = is.null(files),
  force = FALSE,
  # args to wflow_build
  update = FALSE,
  republish = FALSE,
  # general
  dry_run = FALSE,
  project = "."
  ) {
  # To do:
  # * Warning for cache directories
  # * Warning if files in docs/ included
  # Check for modifications to _site.yml. Refuse to build if it is modified

  # Check input arguments ------------------------------------------------------

  if (!is.null(files)) {
    if (!is.character(files)) {
      stop("files must be NULL or a character vector of filenames")
    } else if (!all(file.exists(files))) {
      stop("Not all files exist. Check the paths to the files")
    }
  }

  if (is.null(message)) {
    function_call <- sys.call()
    message <- utils::capture.output(function_call)
  } else if (is.character(message)) {
    message <- wrap(paste(message, collapse = " "))
  } else {
    stop("message must be NULL or a character vector")
  }

  if (!(is.logical(all) && length(all) == 1))
    stop("all must be a one-element logical vector")

  if (!(is.logical(force) && length(force) == 1))
    stop("force must be a one-element logical vector")

  if (!(is.logical(update) && length(update) == 1))
    stop("update must be a one-element logical vector")

  if (!(is.logical(republish) && length(republish) == 1))
    stop("republish must be a one-element logical vector")

  if (!(is.logical(dry_run) && length(dry_run) == 1))
    stop("dry_run must be a one-element logical vector")

  if (is.character(project) && length(project) == 1) {
    if (dir.exists(project)) {
      project <- normalizePath(project)
    } else {
      stop("project directory does not exist.")
    }
  } else {
    stop("project must be a one-element character vector")
  }

  # Step 1: Commit analysis files ----------------------------------------------

  if (dry_run)
    message("Stage 1: Commit analysis files")

  f_committed <- wflow_commit_(files = files, message = message,
                               all = all, force = force,
                               dry_run = dry_run, project = project)

  # Step 2: Build HTML files----------------------------------------------------

  if (dry_run)
    message("Step 3: Build HTML files")

  f_built <- wflow_build_(files = f_committed, make = FALSE,
                          update = update, republish = republish,
                          local = FALSE, dry_run = dry_run, project = project)

  # Step 3 : Commit HTML files -------------------------------------------------

  if (dry_run)
    message("Step 2: Commit HTML files")

  f_committed_site <- wflow_commit_(files = f_built, message = "Build site.",
                                    all = FALSE, force = force,
                                    dry_run = dry_run, project = project)
  f_committed_all <- sort(c(f_committed, f_committed_site))

  return(invisible(f_committed_all))
}
