#' Update a workflowr project.
#'
#' Newer versions of workflowr sometimes make changes that need to be
#' coordinated across multiple files. After upgrading workflowr, run
#' \code{wflow_update} to make all the necessary changes.
#'
#' By default, \code{wflow_update} is run in \code{dry_run} mode so that no
#' unwanted changes are made. The log file contains the changes to each file,
#' represented with the syntax from the
#' \href{https://en.wikipedia.org/wiki/Diff_utility#Usage}{Unix diff utility}.
#' After reviewing the log file for the proposed changes, re-run the function
#' with \code{dry_run = FALSE} to implement them.
#'
#' Currently \code{wflow_update} checks for the following items:
#'
#' \itemize{
#'
#' \item Updates the shared chunks in \code{analysis/chunks.R}.
#'
#' \item Updates each R Markdown file in \code{analysis/} to use the shared
#' chunks. This is implemented with \code{\link{wflow_convert}}.
#'
#' \item Removes the "BuildType: Website" from the Rproj file. This re-builds
#' every R Markdown file everytime, so it is safer to always use
#' \code{\link{wflow_build}}.
#'
#' }
#'
#' @param dry_run logical (default: TRUE). Preview the proposed updates.
#' @param commit logical (default: TRUE). Commit the updated files (only files
#'   tracked by Git are included in commit). Only executed if \code{dry_run =
#'   FALSE}.
#' @param log_file character (default: NULL). A file to save the log messages.
#'   If NULL, a temporary file is created.
#' @param log_open logical (default: \code{interactive()}). Should the log file
#'   be opened in RStudio? This argument is ignored if the function is not run
#'   from within RStudio.
#' @param project character (default: ".") By default the function assumes the
#'   current working directory is within the project. If this is not true,
#'   you'll need to provide the path to the project directory.
#'
#' @return A character vector of the updated files.
#'
#' @seealso \code{\link{wflow_convert}}
#'
#' @examples
#' \dontrun{
#'
#' # Preview the potential changes
#' wflow_update()
#' # Incorporate the changes
#' wflow_update(dry_run = FALSE)
#' }
#'
#' @export
wflow_update <- function(dry_run = TRUE,
                         commit = TRUE,
                         log_file = NULL,
                         log_open = interactive(),
                         project = ".") {

  # Check input arguments ------------------------------------------------------

  if (!(is.logical(dry_run) && length(dry_run) == 1))
    stop("dry_run must be a one element logical vector. You entered: ", dry_run)
  if (!(is.logical(commit) && length(commit) == 1))
    stop("commit must be a one element logical vector. You entered: ", commit)
  if (!(is.null(log_file) || (is.character(log_file) && length(log_file) == 1)))
    stop("log_file must be NULL or a one element character vector. You entered: ",
         log_file)
  if (!(is.logical(log_open) && length(log_open) == 1))
    stop("log_open must be a one element logical vector. You entered: ", log_open)
  if (!(is.character(project) && length(project) == 1))
    stop("project must be a one element character vector. You entered: ", project)
  if (!dir.exists(project))
    stop("project does not exist. You entered: ", project)

  if (dry_run) {
    message("Running wflow_update in dry run mode")
  } else {
    message("Running wflow_update")
  }

  # Start log file -------------------------------------------------------------

  if (is.null(log_file)) {
    log_file <- tempfile("log-wflow-update-", fileext = ".txt")
  } else if (file.exists(log_file)) {
    warning("Overwriting log file: ", log_file)
  }
  message("\nWriting log output to ", log_file, "\n")

  # Start log file
  cat("Log for wflow_update\n", file = log_file)
  cat("\nDate:", as.character(Sys.time()), "\n",
      file = log_file, append = TRUE)

  # Output current version of workflowr
  current_vers <- as.character(utils::packageVersion("workflowr"))
  cat("\nThe current installed version of workflowr is", current_vers, "\n",
      file = log_file, append = TRUE)

  # Output explanation of diff
  cat(diff_explained, file = log_file, append = TRUE)

  if (log_open && rstudioapi::isAvailable()) {
    on.exit(rstudioapi::navigateToFile(log_file))
  }

  # Setup ----------------------------------------------------------------------

  p <- wflow_paths(project = project)
  # Keep track of updated files
  files_updated <- character()

  # Update chunks.R ------------------------------------------------------------

  # Is there a difference between between the chunks.R in this project
  # and that which is currently available in the package?
  chunks_current <- file.path(p$analysis, "chunks.R")
  chunks_pkg <- system.file("infrastructure/analysis/chunks.R",
                            package = "workflowr")
  diffs <- diff_file(from = chunks_current, to = chunks_pkg)
  if (length(diffs) > 0) {
    files_updated <- c(files_updated, chunks_current)
    cat("\nChanges to analysis/chunks.R\n",
        diffs, sep = "\n",
        file = log_file, append = TRUE)
    if (!dry_run) {
      file.copy(from = chunks_pkg, to = chunks_current, overwrite = TRUE)
    }
  } else {
    cat("\nNo changes to analysis/chunks.R\n", sep = "\n",
        file = log_file, append = TRUE)
  }

  # Update Rproj file ----------------------------------------------------------

  # Remove BuildType
  rproj_file <- list.files(path = p$root, pattern = "Rproj$",
                           full.names = TRUE)
  if (length(rproj_file) != 1) {
    cat("\nUnable to locate a single Rproj file\n")
  } else {
    rproj_lines <- readLines(rproj_file)
    website_lines <- grepl("Website", rproj_lines)
    if (sum(website_lines) == 0) {
      cat(sprintf("\nNo changes to %s\n", rproj_file), sep = "\n",
          file = log_file, append = TRUE)
    } else {
      files_updated <- c(files_updated, rproj_file)
      rproj_tmp <- tempfile("rproj-", fileext = "Rproj")
      rproj_lines_new <- rproj_lines[!website_lines]
      cat(rproj_lines_new, sep = "\n", file = rproj_tmp)
      rproj_diffs <- diff_file(from = rproj_file, to = rproj_tmp)
      cat(sprintf("\nChanges to %s\n", rproj_file), rproj_diffs,
          sep = "\n", file = log_file, append = TRUE)
      if (!dry_run) {
        file.copy(from = rproj_tmp, to = rproj_file, overwrite = TRUE)
      }
    }
  }

  # Gather all R Markdown analysis files ---------------------------------------

  rmd_all <- list.files(path = p$analysis, pattern = "^[^_].*\\.[Rr]md$",
                        full.names = TRUE)
  # Remove index.Rmd, about.Rmd, and license.Rmd
  rmd_remove <- file.path(p$analysis,
                          c("index.Rmd", "about.Rmd", "license.Rmd"))
  rmd_to_convert <- setdiff(rmd_all, rmd_remove)
  # Remove any files which are untracked or have staged (or staged) changes
  if (!is.na(p$git)) {
    r <- git2r::repository(p$git)
    status <- git2r::status(r, ignored = TRUE)
    git_all <- unlist(status)
    git_rmd <- git_all[grepl("[Rd]md$", git_all)]
    if (length(git_rmd) > 0) {
      rmd_to_convert <- setdiff(rmd_to_convert, git_rmd)
      cat("", strwrap(
"The following R Markdown files are not converted because they are untracked
or have staged (or unstaged) changes. To convert, first commit the
changes and then re-run `wflow_update`:"
      ), "", git_rmd, sep = "\n", file = log_file, append = TRUE)
    }
  }

  # Convert Rmd files ----------------------------------------------------------

  # Attempt conversion one-by-one so that errors do not break it.
  for (rmd in rmd_to_convert) {
    rmd_result <- tryCatch(
      rmd_diffs <- wflow_convert(rmd, dry_run = dry_run, verbose = FALSE),
      error = function(e) "error")
    if (rmd_result == "error") {
      cat("\nUnable to convert ", rmd, ". Please inspect manually.\n",
          sep = "", file = log_file, append = TRUE)
    } else if (length(rmd_diffs[[1]]) > 0) {
      files_updated <- c(files_updated, rmd)
      cat(sprintf("\nChanges to %s:\n", rmd),
          rmd_diffs[[1]], sep = "\n", file = log_file, append = TRUE)
    } else {
      cat("\nNo changes to", rmd, "\n", file = log_file, append = TRUE)
    }
  }

  if (length(files_updated) > 0) {
    cat("\nList of updated files:\n", files_updated, sep = "\n",
        file = log_file, append = TRUE)
  } else {
    cat("\nNo updated files\n", sep = "\n",
        file = log_file, append = TRUE)
  }

  # Commit updated files (tracked files only) ----------------------------------

  if (!dry_run & commit & length(files_updated) > 0 & !is.na(p$git)) {
    cat("\nAttempting to commit changes\n",
        file = log_file, append = TRUE)
    r <- git2r::repository(p$git)
    status <- git2r::status(r)
    if (length(status$staged) > 0) {
      warning("\nFiles already in staging area were included in commit:\n",
              paste(unlist(status$staged), collapse = "\n"))
    }
    git2r::add(r, path = files_updated)
    status <- git2r::status(r)
    if (length(unlist(status$staged)) > 0) {
      git2r::commit(r, message = sprintf(
        "Update workflowr project with wflow_update (version %s).",
        current_vers))
      last_commit <- git2r::commits(r)[[1]]
      cat("Changes committed",
          utils::capture.output(methods::show(last_commit)),
          sep = "\n\n", file = log_file, append = TRUE)
    } else {
      cat("\nUnable to commit changes for unknown reason.",
          "Manual intervention required.\n",
          file = log_file, append = TRUE)
    }
  }

  return(invisible(files_updated))
}

diff_explained <- paste(strwrap(
  "\n
Explanation of diff output: The changes to the files are displayed using the
notation from the diff command-line utility. A \">\" indicates an inserted
line, and a \"<\" indicates a deleted line.

https://en.wikipedia.org/wiki/Diff_utility#Usage
\n
  "
  ), collapse = "\n")
