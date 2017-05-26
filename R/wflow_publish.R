#' Publish the site
#'
#' \code{wflow_publish} is the main workflowr function. Use it when you are
#' ready to publish an analysis to your site. \code{wflow_publish} performs
#' three steps: 1) commit the file(s), 2) rebuild the file(s), 3) commit the
#' generated website file(s). These steps ensure that the version of the HTML
#' file is created by the latest version of the R Markdown file, which is
#' critical for reproducibility.
#'
#' @inheritParams wflow_commit
#' @inheritParams wflow_build
#'
#' @return Invisibly returns the full path to the R Markdown file(s).
#'
#' @seealso \code{\link{wflow_commit}}, \code{\link{wflow_build}}
#'
#' @examples
#' \dontrun{
#' # single file
#' wflow_publish("analysis/file.Rmd")
#' # All tracked files that have been updated
#' wflow_publish(all = TRUE)
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
#' @export
wflow_publish <- function(
  # args to wflow_commit
  files = NULL,
  message = NULL,
  all = FALSE,
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
    # Change filepaths to relative paths
    files <- sapply(files, relpath)
  }

  if (is.null(message)) {
    message <- deparse(sys.call())
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

  # Assess project status ------------------------------------------------------

  s0 <- wflow_status(project = project)
  r <- git2r::repository(path = s$git)
  commit_current <- git2r::commits(r, n = 1)[[1]]

  # Step 1: Commit analysis files ----------------------------------------------

  # Decide if wflow_commit should be run. At least one of the following
  # scenarios must be true:
  #
  # 1) Rmd files were specified and at least one has unstaged/staged changes
  #
  # 2) `all == TRUE` and at least one tracked file as unstaged/staged changes
  #
  # 3) At least one non-Rmd file was specified
  s1_scenario1 <- !is.null(files) &&
    any(unlist(s0$status[files, c("mod_unstaged", "mod_staged")]),
        na.rm = TRUE)
  s1_scenario2 <- all &&
    any(unlist(s0$status[s$status$tracked, c("mod_unstaged", "mod_staged")]),
        na.rm = TRUE)
  s1_scenario3 <- !is.null(files) &&
    any(!(files %in% rownames(s0$status)))

  if (s1_scenario1 || s1_scenario2 || s1_scenario3) {
    step1 <- wflow_commit(files = files, message = message,
                          all = all, force = force,
                          dry_run = dry_run, project = project)
    # If subsequent steps fail, undo this action by resetting the Git repo to
    # its initial state.
    on.exit(git2r::reset(commit_current, reset_type = "mixed"), add = TRUE)
    s1 <- wflow_status(project = project)
  } else {
    step1 <- NULL
    s1 <- s0
  }

  # Step 2: Build HTML files----------------------------------------------------

  # Determine if there are any files to be built.
  files_to_build <- character()
  # Specified files
  files_to_build <- union(files_to_build,
                          files[files %in% rownames(s1$status)])
  # Files committed in Step 1
  files_to_build <- union(files_to_build,
                          step1$commit_files[
                            step1$commit_files %in% rownames(s1$status)])
  # If `republish == TRUE`, all published files
  if (republish) {
    files_to_build <- union(files_to_build,
                            rownames(s1$status)[s1$status$published])
  }
  # If `update == TRUE`, all published files with committed modifications
  if (update) {
    files_to_build <- union(files_to_build,
                            rownames(s1$status)[s1$status$mod_committed])
  }
  # None of these files can have unstaged/staged changes
  files_to_build <- files_to_build[!s1$status[files_to_build, "mod_unstaged"]]
  files_to_build <- files_to_build[!s1$status[files_to_build, "mod_staged"]]

  if (length(files_to_build) > 0) {
    # As a backup, copy the docs/ directory to /tmp
    docs_backup <- tempfile(pattern = sprintf("docs-backup-%s-",
                                              format(Sys.time(),
                                                     "%Y-%m-%d-%Hh-%Mm-%Ss")))
    dir.create(docs_backup)
    file.copy(from = s1$docs, to = docs_backup, recursive = TRUE)
    step2 <- wflow_build_(files = files_to_build, make = FALSE,
                          update = update, republish = republish,
                          local = FALSE, dry_run = dry_run, project = project)
    # If something fails in subsequent steps, delete docs/ and restore backup
    on.exit(unlink(s1$docs, recursive = TRUE), add = TRUE)
    on.exit(file.copy(from = docs_backup, to = s1$docs, recursive = TRUE),
            add = TRUE)
    s2 <- wflow_status(project = project)
  } else {
    step2 <- NULL
    s2 <- s1
  }

  # Step 3 : Commit HTML files -------------------------------------------------

  if (dry_run)
    message("Step 2: Commit HTML files")

  f_committed_site <- wflow_commit_(files = f_built, message = "Build site.",
                                    all = FALSE, force = force,
                                    dry_run = dry_run, project = project)
  f_committed_all <- sort(c(f_committed, f_committed_site))

  # Prepare output -------------------------------------------------------------

  o <- list(step1, step2)
  class(o) <- "wflow_publish"

  # If everything worked, erase the on.exit code that would have reset
  # everything.
  on.exit()

  return(o)
}

#' @export
print.wflow_publish <- function(x, ...) {
  cat("wflow_publish\n\n")

  cat("Step 1: Commit files\n\n")
  if (is.null(x$step1)) {
    cat("No files to commit\n\n")
  } else {
    print(x$step1)
  }

  cat("Step 2: Build HTML files\n\n")
  if (is.null(x$step2)) {
    cat("No files to build\n\n")
  } else {
    print(x$step2)
  }

  return(invisible(x))
}
