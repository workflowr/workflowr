#' Open an R Markdown analysis file
#'
#' \code{wflow_open} opens an R Markdown file in RStudio and sets the working
#' directory to \code{analysis/}. If the file does not exist, it creates a new
#' Rmd using the workflowr template.
#'
#' \code{wflow_open} is a convenience function to make it easier to start working,
#' especially when starting a new analysis. It will create the new file if
#' necessary, set the working directory to \code{analysis/}, and open the file
#' in RStudio. The latter two side effects can be turned off if desired.
#'
#' If you are not using RStudio and are comfortable with relative paths and
#' managing your working directory, you could instead use directly use the
#' underlying commands. First set the working directory to the \code{analysis/}
#' subdirectory of your workflowr project. Second create the new file with
#' \code{draft("model-data.Rmd", template = "analysis", package = "workflowr",
#' edit = FALSE)}. Third open the file in your text editor.
#'
#' @param filename The basename of the R Markdown file.
#' @param change_wd Should the working directory be changed to the analysis
#'   subdirectory? (default: TRUE)
#' @param open_file Should the file be opened in RStudio editor? (default: TRUE)
#' @param path By default the function assumes the current working directory is
#'   within the project. If this is not true, you'll need to provide the path to
#'   the project directory.
#'
#' @return Invisibly returns the full path to the R Markdown file.
#'
#' @examples
#' \dontrun{
#' wflow_open("model-data.Rmd")
#' }
#' @export
wflow_open <- function(filename,
                     change_wd = TRUE,
                     open_file = TRUE,
                     path = ".") {
  filename_base <- basename(filename)
  if (filename != filename_base) {
    message("filename should just be the basename.")
    message(sprintf("e.g. %s", filename_base))
    message(sprintf("not %s", filename))
    message("It will be saved in analysis/")
    stop(sprintf("wflow_open: Invalid filename argument"))
  }
  analysis_dir <- rprojroot::find_rstudio_root_file("analysis", path = path)
  rmd_filename <- file.path(analysis_dir, filename)
  # Create the file if it doesn't exist
  if (!file.exists(rmd_filename)) {
    rmarkdown::draft(rmd_filename, template = "analysis", package = "workflowr",
                     edit = FALSE)
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
    rstudioapi::navigateToFile(rmd_filename)
  }

  return(invisible(rmd_filename))
}
