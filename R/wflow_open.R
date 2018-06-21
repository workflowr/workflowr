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

    # Find knit directory (knit_root_dir). This ignores any potential settings
    # in the YAML header(s) of existing file(s).
    wflow_opts <- wflow_options(files[1])
    knit_directory <- wflow_opts$knit_root_dir
    if (is.null(knit_directory)) {
      knit_directory <- dirname(files)
    }
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

  for (i in seq_along(files)) {
    if (!file.exists(files[i])) {
      header <- glue::glue("---
                           title: \"{yaml_title[i]}\"
                           author: \"{yaml_author}\"
                           date: \"{yaml_date}\"
                           output: workflowr::wflow_html
                           ---")
      writeLines(header, files[i])
    }
  }

  # Set working directory ------------------------------------------------------

  current_wd <- getwd()
  if (change_wd & current_wd != knit_directory) {
    setwd(knit_directory)
    message("Working directory was changed.",
            "\nPrevious:\t", absolute(current_wd),
            "\nCurrent:\t", absolute(knit_directory))
  }

  # Open files -----------------------------------------------------------------

  if (rstudioapi::isAvailable() & open_file) {
    for (rmd in files) {
      rstudioapi::navigateToFile(rmd)
    }
  }

  # Prepare output -------------------------------------------------------------

  return(invisible(relative(files)))
}
