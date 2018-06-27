#' Open R Markdown analysis file(s)
#'
#' \code{wflow_open} opens R Markdown files in RStudio and sets the working
#' directory to the knit directory (see Details). If a file does not exist, a
#' minimal one is created.
#'
#' \code{wflow_open} is a convenience function to make it easier to begin
#' working, especially when starting a new analysis. First, it creates a new
#' file if necessary and tries to make educated guesses about metadata like the
#' title, author, and date. Second, it sets the working directory to the knit
#' directory. The knit directory is where the code in the R Markdown files is
#' executed, and may be defined via the field \code{knit_root_dir} in the file
#' \code{_workflowr.yml} (see \code{\link{wflow_html}} for all the details). If
#' this field is not defined, then the knit directory is the R Markdown
#' directory. Third, it opens the file(s) in RStudio if applicable. The latter
#' two side effects can be turned off if desired.
#'
#' If you would like to create an R Markdown file with \code{wflow_open} for an
#' analysis that is not part of a workflowr project, set \code{project =
#' NULL}. Otherwise \code{wflow_open} will throw an error.
#'
#' @param files character. R Markdown file(s) to open. Files must have the
#'   extension Rmd or rmd. Supports file
#'   \href{https://en.wikipedia.org/wiki/Glob_(programming)}{globbing}. Set
#'   \code{project = NULL} to override to create an R Markdown file outside of a
#'   workflowr project.
#' @param change_wd logical (default: TRUE). Change the working directory to the
#'   knit directory. If \code{project = NULL}, change working directory to
#'   destination of first file in \code{files}.
#' @param open_file logical (default: TRUE). Open the file in the RStudio
#'   editor.
#' @param project character (or NULL). By default the function assumes the
#'   current working directory is within the project. If this is not true,
#'   you'll need to provide the path to the project directory. Set \code{project
#'   = NULL} if running this command to create a file for a non-workflowr
#'   project.
#'
#' @return Invisibly returns the relative path(s) to the R Markdown file(s).
#'
#' @examples
#' \dontrun{
#' wflow_open("analysis/model-data.Rmd")
#' # Multiple files
#' wflow_open(c("analysis/model-data.Rmd", "analysis/another-analysis.Rmd"))
#' # Open all R Markdown files
#' wflow_open("analysis/*Rmd")
#' # Create an R Markdown file in a non-worklowr project
#' wflow_open("model-data.Rmd", project = NULL)
#' }
#' @import rmarkdown
#' @export
wflow_open <- function(files,
                       change_wd = TRUE,
                       open_file = TRUE,
                       project = ".") {

  # Check input arguments ------------------------------------------------------

  if (!is.character(files))
    stop("files must be a character vector")
  if (!(is.logical(change_wd) && length(change_wd) == 1))
    stop("change_wd must be a one element logical vector")
  if (!(is.logical(open_file) && length(open_file) == 1))
    stop("open_file must be a one element logical vector")
  if (!(is.null(project) || (is.character(project) && length(project) == 1)))
    stop("project must be NULL or a one element character vector")

  files <- glob(files)
  files <- absolute(files)
  project <- absolute(project)

  # Check file extensions
  extensions <- tools::file_ext(files)
  non_standard <- !(extensions %in% c("Rmd", "rmd"))
  if (any(non_standard))
    stop("R Markdown files must have the extension Rmd or rmd.")

  # Determine knit directory ---------------------------------------------------

  # If project is NULL, create upstream directories. Set `knit_directory` to
  # the directory of the first file for `change_wd`.
  if (is.null(project)) {
    for (rmd_dir in dirname(files)) {
      dir.create(rmd_dir, showWarnings = FALSE, recursive = TRUE)
    }
    knit_directory <- dirname(files)[1]
  } else {
    # If project is set, find the knit directory

    # Confirm that project exists
    if (!dir.exists(project)) {
      stop("project does not exist: ", project)
    }

    # Confirm project is a valid workflowr project
    tryCatch(p <- wflow_paths(project = project),
             error = function(e) {
               stop(call. = FALSE, wrap(
                 "This isn't a workflowr project. Set project=NULL to create
                 an R Markdown file outside of a workflowr project. See
                 ?wflow_open for details."
               ))
             }
    )

    # Confirm that the Rmd files to be created are in the workflowr project
    regex_root <- paste0("^", absolute(p$root))
    outside <- !stringr::str_detect(files, regex_root)
    if (any(outside)) {
      stop(call. = FALSE, wrap(
        "The following file(s) are not within the workflowr project. Set
        project=NULL to create an R Markdown file outside of a workflowr
        project. See ?wflow_open for details."), "\n\n",
        paste(files[outside], collapse = "\n")
      )
    }

    # Send warning if Rmd files not saved in R Markdown directory
    regex_analysis <- paste0("^", absolute(p$analysis))
    non_analysis <- !stringr::str_detect(files, regex_analysis)
    if (any(non_analysis)) {
      warning(call. = FALSE, wrap(
        "The following file(s) are not within the R Markdown directory of your
        workflowr project. This probably isn't what you wanted."), "\n\n",
        paste(files[non_analysis], collapse = "\n"), "\n\n",
        wrap(
          "For the results to be included in the website, the R Markdown files
          need to be saved in the following directory:"),  "\n\n",
        absolute(p$analysis)
      )
    }

    # Find knit directory (knit_root_dir). This ignores any potential settings
    # in the YAML header(s) of existing file(s).
    wflow_opts <- wflow_options(files[1])
    knit_directory <- wflow_opts$knit_root_dir
    if (is.null(knit_directory)) {
      knit_directory <- p$analysis
    }
    knit_directory <- absolute(knit_directory)
  }

  # Guess metadata -------------------------------------------------------------

  yaml_title <- tools::file_path_sans_ext(basename(files))

  yaml_author <- ""
  git_config <- git2r::config()
  if (!is.null(git_config$global$user.name)) {
    yaml_author <-  git_config$global$user.name
  }
  if (!is.null(git_config$local$user.name)) {
    yaml_author <-  git_config$local$user.name
  }

  yaml_date <- as.character(Sys.Date())

  # Create files ---------------------------------------------------------------

  files_new <- files[!file.exists(files)]
  for (i in seq_along(files_new)) {
    header <- glue::glue("---
                           title: \"{yaml_title[i]}\"
                           author: \"{yaml_author}\"
                           date: \"{yaml_date}\"
                           output: workflowr::wflow_html
                           ---")
    writeLines(header, files_new[i])
  }

  # Now that files all exist, ensure that symlinks are expanded
  files <- absolute(files)
  files_new <- absolute(files_new)

  # Set working directory ------------------------------------------------------

  current_wd <- absolute(getwd())
  if (change_wd && current_wd != knit_directory) {
    setwd(knit_directory)
    new_wd <- knit_directory
  } else {
    new_wd <- NULL
  }

  # Open files -----------------------------------------------------------------

  if (rstudioapi::isAvailable() && open_file) {
    for (rmd in files) {
      rstudioapi::navigateToFile(rmd)
    }
  }

  # Prepare output -------------------------------------------------------------

  o <- list(files = files,
            change_wd = change_wd,
            open_file = open_file,
            knit_root_dir = knit_directory,
            previous_wd  = current_wd,
            new_wd = new_wd,
            files_new = files_new)

  class(o) <- "wflow_open"

  return(o)
}

#' @export
print.wflow_open <- function(x, ...) {
  cat("wflow_open:\n")
  if (length(x$files_new) > 0) {
    cat("- New file(s):\n")
    cat(paste0("  ", x$files_new), sep = "\n")
  }
  files_existing <- setdiff(x$files, x$files_new)
  if (length(files_existing) > 0) {
    cat("- Existing file(s):\n")
    cat(paste0("  ", files_existing), sep = "\n")
  }

  if (x$change_wd && !is.null(x$new_wd)) {
    cat(sprintf("- New working directory: %s\n", x$new_wd))
  } else {
    cat(sprintf("- Same working directory: %s\n", x$previous_wd))
  }

  return(invisible(x))
}
