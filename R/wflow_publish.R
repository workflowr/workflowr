#' Publish the site
#'
#' \code{wflow_publish} is the main workflowr function. Use it when you are
#' ready to publish an analysis to your site. \code{wflow_publish} performs
#' three steps: 1) commit the file(s) (can include both Rmd and non-Rmd files,
#' e.g. \code{_site.yml}), 2) rebuild the R Markdown file(s), 3) commit the
#' generated website file(s). These steps ensure that the version of the HTML
#' file is created by the latest version of the R Markdown file, which is
#' critical for reproducibility.
#'
#' @param files character (default: NULL). R Markdown files and other files to
#'   be added and committed with Git (step 1). Any R Markdown files will also be
#'   built (step 2) and their output HTML and figures will be subsequently
#'   committed (step 3). Supports file
#'   \href{https://en.wikipedia.org/wiki/Glob_(programming)}{globbing}.
#'   The files are always built in the order they are listed.
#' @inheritParams wflow_git_commit
#' @inheritParams wflow_build
#'
#' @return Returns an object of class \code{wflow_publish}, which is a list with
#'   the following elements:
#'
#'   \itemize{
#'
#'   \item \bold{step1}: An object of class \code{wflow_git_commit} from the first
#'   step of committing the files.
#'
#'   \item \bold{step2}: An object of class \code{wflow_build} from the second
#'   step of building the HTML files.
#'
#'   \item \bold{step3}: An object of class \code{wflow_git_commit} from the third
#'   step of committing the HTML files.
#'
#'   }
#'
#' @seealso \code{\link{wflow_git_commit}}, \code{\link{wflow_build}}
#'
#' @examples
#' \dontrun{
#' # single file
#' wflow_publish("analysis/file.Rmd", "Informative commit message")
#' # All tracked files that have been edited
#' wflow_publish(all = TRUE, message = "Informative commit message")
#' # A new file plus all tracked files that have been edited
#' wflow_publish("analysis/file.Rmd", "Informative commit message", all = TRUE)
#' # Multiple files
#' wflow_publish(c("analysis/file.Rmd", "analysis/another.Rmd"),
#'               "Informative commit message")
#' # All R Markdown files that start with the pattern "new_"
#' wflow_publish("analysis/new_*Rmd", "Informative commit message")
#' # Republish all published files even though they haven't been modified.
#' # Useful for changing some universal aspect of the site, e.g. the theme
#' # specified in _site.yml.
#' wflow_publish("analysis/_site.yml", "Informative commit message",
#'               republish = TRUE)
#' # Publish all previously published files that have been committed more
#' # recently than their corresponding HTML files. This is useful if you like to
#' # manually commit your R Markdown files.
#' wflow_publish(update = TRUE)
#' }
#'
#' @import rmarkdown
#' @export
wflow_publish <- function(
  # args to wflow_git_commit
  files = NULL,
  message = NULL,
  all = FALSE,
  force = FALSE,
  # args to wflow_build
  update = FALSE,
  republish = FALSE,
  view = getOption("workflowr.view"),
  delete_cache = FALSE,
  seed = 12345,
  verbose = FALSE,
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
    if (!(is.character(files) && length(files) > 0))
      stop("files must be NULL or a character vector of filenames")
    files <- glob(files)
    if (!all(fs::file_exists(files)))
      stop("Not all files exist. Check the paths to the files")
    # Change filepaths to relative paths
    files <- relative(files)
  }

  if (is.null(message)) {
    message <- deparse(sys.call())
    message <- paste(message, collapse = "\n")
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

  if (!(is.logical(view) && length(view) == 1))
    stop("view must be a one-element logical vector")

  if (!(is.logical(delete_cache) && length(delete_cache) == 1))
    stop("delete_cache must be a one-element logical vector")

  if (!(is.numeric(seed) && length(seed) == 1))
    stop("seed must be a one element numeric vector")

  if (!(is.logical(verbose) && length(verbose) == 1))
    stop("verbose must be a one-element logical vector")

  if (!(is.logical(dry_run) && length(dry_run) == 1))
    stop("dry_run must be a one-element logical vector")

  if (!(is.character(project) && length(project) == 1))
    stop("project must be a one-element character vector")

  check_wd_exists()

  if (!fs::dir_exists(project)) {
    stop("project directory does not exist.")
  }

  project <- absolute(project)

  if (isTRUE(getOption("workflowr.autosave"))) autosave()

  # Assess project status ------------------------------------------------------

  s0 <- wflow_status(project = project)
  r <- git2r::repository(path = s0$git)
  commit_current <- git2r::commits(r, n = 1)[[1]]

  if (!dry_run) check_git_config(project, "`wflow_publish`")

  # Step 1: Commit analysis files ----------------------------------------------

  # Decide if wflow_git_commit should be run. At least one of the following
  # scenarios must be true:
  #
  # 1) Rmd files were specified and at least one is scratch (untracked) or has
  # unstaged/staged changes
  #
  # 2) `all == TRUE` and at least one tracked file has unstaged/staged changes
  #
  # 3) At least one non-Rmd file was specified
  scenario1 <- !is.null(files) &&
    any(unlist(s0$status[files, c("mod_unstaged", "mod_staged", "scratch")]),
        na.rm = TRUE)
  scenario2 <- all &&
    any(unlist(s0$status[s0$status$tracked, c("mod_unstaged", "mod_staged")]),
        na.rm = TRUE)
  scenario3 <- !is.null(files) &&
    any(!(files %in% rownames(s0$status)))

  if (scenario1 || scenario2 || scenario3) {
    step1 <- wflow_git_commit(files = files, message = message,
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
    # Create a backup copy of the docs/ directory. If either step 2 (build the
    # HTML) or step 3 (commit the HTML) fails, delete docs/ and restore backup
    if (fs::dir_exists(s1$docs) && !dry_run) {
      docs_backup <- tempfile(pattern = sprintf("docs-backup-%s-",
                                                format(Sys.time(),
                                                       "%Y-%m-%d-%Hh-%Mm-%Ss")))
      fs::dir_create(docs_backup)
      docs_backup <- absolute(docs_backup)
      file.copy(from = file.path(s1$docs, "."), to = docs_backup,
                recursive = TRUE, copy.date = TRUE)
      on.exit(unlink(s1$docs, recursive = TRUE), add = TRUE)
      on.exit(fs::dir_create(s1$docs), add = TRUE)
      on.exit(file.copy(from = file.path(docs_backup, "."), to = s1$docs,
                        recursive = TRUE, copy.date = TRUE), add = TRUE)
    }

    step2 <- wflow_build(files = files_to_build, make = FALSE,
                         update = update, republish = republish,
                         view = view, clean_fig_files = TRUE,
                         delete_cache = delete_cache, seed = seed,
                         local = FALSE, verbose = verbose,
                         dry_run = dry_run, project = project)
  } else {
    step2 <- NULL
  }

  # Step 3 : Commit HTML files -------------------------------------------------

  # Step 3 only needs to be performed if files were built in step 2.
  if (length(step2$built) > 0) {

    # Have to loop on step2$built as an underlying git2r function requires a
    # length 1 character vector
    figs_path <- vapply(step2$built, create_figure_path, character(1))
    dir_figure <- file.path(s0$docs, figs_path)
    site_libs <- file.path(s0$docs, "site_libs")
    docs_nojekyll <- file.path(s0$docs, ".nojekyll")
    docs_css <- list.files(path = s0$docs, pattern = "css$", full.names = TRUE)
    docs_js <- list.files(path = s0$docs, pattern = "js$", full.names = TRUE)
    files_to_commit <- c(step2$html, dir_figure, site_libs, docs_nojekyll,
                         docs_css, docs_js)

    # Call directly to internal function `wflow_git_commit_` to bypass input checks.
    # In a dry run, some files may not actually exist yet. Also, not every Rmd
    # file creates figures, but it's easier to just attempt to add figures for
    # every file.
    step3 <- wflow_git_commit_(files = files_to_commit, message = "Build site.",
                          all = FALSE, force = force,
                          dry_run = dry_run, project = project)
  } else {
    step3 <- NULL
  }

  # Prepare output -------------------------------------------------------------

  o <- list(step1 = step1, step2 = step2, step3 = step3)
  class(o) <- "wflow_publish"

  # If everything worked, erase the on.exit code that would have reset
  # everything.
  on.exit()

  return(o)
}

#' @export
print.wflow_publish <- function(x, ...) {
  cat("Summary from wflow_publish\n\n")

  cat("**Step 1: Commit analysis files**\n\n")
  if (is.null(x$step1)) {
    cat("No files to commit\n\n")
  } else {
    print(x$step1)
  }

  cat("\n**Step 2: Build HTML files**\n\n")
  if (is.null(x$step2)) {
    cat("No files to build\n\n")
  } else {
    print(x$step2)
  }

  cat("\n**Step 3: Commit HTML files**\n\n")
  if (is.null(x$step3)) {
    cat("No HTML files to commit\n\n")
  } else {
    print(x$step3)
  }

  return(invisible(x))
}
