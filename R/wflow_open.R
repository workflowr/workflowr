#' Open R Markdown analysis file(s)
#'
#' \code{wflow_open} opens R Markdown files in RStudio and sets the working
#' directory to \code{analysis/}. If a file does not exist, it is created using
#' the workflowr R Markdown template.
#'
#' \code{wflow_open} is a convenience function to make it easier to start
#' working, especially when starting a new analysis. It will create a new file
#' if necessary, set the working directory to \code{analysis/}, and open the
#' file(s) in RStudio. The latter two side effects can be turned off if desired.
#'
#' If you are not using RStudio and are comfortable with relative paths and
#' managing your working directory, you could instead directly use the
#' underlying commands. First set the working directory to the \code{analysis/}
#' subdirectory of your workflowr project. Second create the new file with
#' \code{draft("model-data.Rmd", template = "analysis", package = "workflowr",
#' edit = FALSE)}. Third open the file in your text editor.
#'
#' @param filename The name of the R Markdown file(s) (can be either a path or
#'   just the basename, but will always be written to \code{analysis/}
#'   directory). File extension must be .Rmd.
#' @param change_wd Should the working directory be changed to the analysis
#'   subdirectory? (default: TRUE)
#' @param open_file Should the file be opened in RStudio editor? (default: TRUE)
#' @param path By default the function assumes the current working directory is
#'   within the project. If this is not true, you'll need to provide the path to
#'   the project directory.
#'
#' @return Invisibly returns the full path to the R Markdown file(s).
#'
#' @examples
#' \dontrun{
#' # Any of the following are valid input. Files would be created if they did
#' # not exist.
#' # Basename of file:
#' wflow_open("model-data.Rmd")
#' # Path to file
#' wflow_open("analysis/model-data.Rmd")
#' # Multiple files
#' wflow_open(c("model-data.Rmd", "another-analysis.Rmd"))
#' }
#' @import rmarkdown
#' @export
wflow_open <- function(filename,
                       change_wd = TRUE,
                       open_file = TRUE,
                       path = ".") {
  stopifnot(is.character(filename),
            grepl("Rmd$", filename),
            is.logical(change_wd),
            is.logical(open_file),
            is.character(path))
  analysis_dir <- rprojroot::find_rstudio_root_file("analysis", path = path)
  stopifnot(dir.exists(analysis_dir))
  rmd_filename <- file.path(analysis_dir, basename(filename))

  # Provide warning if user provides a path to a directory other than analysis
  rmd_dirname <- dirname(filename)
  rmd_path <- stringr::str_split(rmd_dirname, .Platform$file.sep)
  rmd_subdir <- sapply(rmd_path, function(x) x[length(x)])
  for (i in seq_along(rmd_filename)) {
    if (!(rmd_subdir[i] == "analysis" | rmd_subdir[i] == ".")) {
      warning(sprintf("Input file had invalid subdirectory specified.\nInput filename: %s\nOutput filename: %s",
                      filename[i], rmd_filename[i]))
    }
  }

  # Create the file(s) if non-existent
  for (rmd in rmd_filename) {
    if (!file.exists(rmd)) {
      rmarkdown::draft(rmd, template = "analysis", package = "workflowr",
                       edit = FALSE)
    }
  }

  # Change the working directory
  current_wd <- getwd()
  if (change_wd & current_wd == analysis_dir) {
    message("Current working directory not changed. Already set to:")
    message(analysis_dir)
  } else if (change_wd) {
    setwd(analysis_dir)
    message("Current working directory changed to:")
    message(analysis_dir)
  }
  # Open in RStudio
  if (rstudioapi::isAvailable() & open_file) {
    for (rmd in rmd_filename) {
      rstudioapi::navigateToFile(rmd)
    }
  }

  return(invisible(rmd_filename))
}
