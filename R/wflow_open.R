#' Open R Markdown analysis file(s)
#'
#' \code{wflow_open} opens R Markdown files in RStudio and sets the working
#' directory to the analysis directory. If a file does not exist, it is created
#' using the workflowr R Markdown template.
#'
#' \code{wflow_open} is a convenience function to make it easier to begin
#' working, especially when starting a new analysis. It will create a new file
#' if necessary using the workflowr template, set the working directory to
#' the analysis directory, and open the file(s) in RStudio. The latter two side
#' effects can be turned off if desired.
#'
#' If you would like to create a file using the workflowr template for an
#' analysis that is not part of a workflowr project, set \code{standalone =
#' TRUE} to directly embed the shared configuration chunks into the document and
#' set \code{project = NULL} to prevent \code{wflow_open} from searching for an
#' analysis directory.
#'
#' If you are not using RStudio and are comfortable with relative paths and
#' managing your working directory, you could instead directly use the
#' underlying commands. First set the working directory to the analysis
#' directory of your workflowr project. Second create the new file with
#' \code{draft("model-data.Rmd", template = "analysis", package = "workflowr",
#' edit = FALSE)}. Third open the file in your text editor.
#'
#' @param files character. The name of the R Markdown file(s). If \code{project}
#'   points to a workflowr project (or any subdirectory) the file(s) will be
#'   saved in the analysis directory. Set \code{project = NULL} to
#'   override this default behavior. Files must have the extension Rmd or rmd.
#' @param change_wd logical (default: TRUE). Change the working directory to the
#'   analysis directory. If \code{project = NULL}, change working
#'   directory to destination of first file in \code{files}.
#' @param open_file logical (default: TRUE). Open the file in the RStudio
#'   editor.
#' @param standalone logical (default: FALSE). Embed the configuration chunks
#'   directly in the document to create a standalone file. Should only need to
#'   be used when creating files outside the context of a workflowr project.
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
#' # Any of the following are valid input. Files would be created if they did
#' # not exist.
#' # Basename of file:
#' wflow_open("model-data.Rmd")
#' # Path to file
#' wflow_open("analysis/model-data.Rmd")
#' # Multiple files
#' wflow_open(c("model-data.Rmd", "another-analysis.Rmd"))
#' # Use the workflowr template in a non-worklowr project
#' wflow_open("model-data.Rmd", standalone = TRUE, project = NULL)
#' }
#' @import rmarkdown
#' @export
wflow_open <- function(files,
                       change_wd = TRUE,
                       open_file = TRUE,
                       standalone = FALSE,
                       project = ".") {
  if (!is.character(files))
    stop("files must be a character vector")
  if (!(is.logical(change_wd) && length(change_wd) == 1))
    stop("change_wd must be a one element logical vector")
  if (!(is.logical(open_file) && length(open_file) == 1))
    stop("open_file must be a one element logical vector")
  if (!(is.logical(standalone) && length(standalone) == 1))
    stop("standalone must be a one element logical vector")
  if (!(is.null(project) || (is.character(project) && length(project) == 1)))
    stop("project must be NULL or a one element character vector")

  # Check file extensions
  extensions <- tools::file_ext(files)
  non_standard <- !(extensions %in% c("Rmd", "rmd"))
  if (any(non_standard))
    stop("R Markdown files must have the extension Rmd or rmd.")

  # If project is NULL, create destination directories if they do not exist. Also
  # set `analysis_dir` to the directory of the first file for `change_wd`.
  #
  # If project is set, find the analysis/ subdirectory. Provide warning
  # if user included a path to a location other than analysis/
  if (is.null(project)) {
    rmd_files <- files
    files_dir <- dirname(rmd_files)
    for (fdir in files_dir) {
      if (!dir.exists(fdir)) {
        message("Creating output directory ", fdir)
        dir.create(fdir, recursive = TRUE)
      }
    }
    analysis_dir <- relpath_vec(files_dir[1])
  } else {
    # Confirm that project exists
    if (!dir.exists(project)) {
      stop("project does not exist: ", project)
    } else {
      project <- normalizePath(project)
    }
    # Find analysis/ subdirectory
    p <- wflow_paths(project = project)
    analysis_dir <- p$analysis
    # Set path to analysis/ for all files
    files_base <- basename(files)
    rmd_files <- file.path(analysis_dir, files_base)
    # Provide warning if user provided a path to a directory other than analysis
    files_dir <- dirname(files)
    files_path <- stringr::str_split(files_dir, .Platform$file.sep)
    files_subdir <- sapply(files_path, function(x) x[length(x)])
    for (i in seq_along(rmd_files)) {
      if (!(files_subdir[i] == "analysis" | files_subdir[i] == ".")) {
        warning("Input file had invalid subdirectory specified.\n",
                sprintf("Input filename: %s\nOutput filename: %s",
                        files[i], rmd_files[i]))
      }
    }
  }

  # Create the file(s) if non-existent
  for (rmd in rmd_files) {
    if (!file.exists(rmd)) {
      if (standalone) {
        rmarkdown::draft(rmd, template = "analysis_standalone",
                         package = "workflowr", edit = FALSE)
      } else {
        rmarkdown::draft(rmd, template = "analysis",
                         package = "workflowr", edit = FALSE)
      }
    }
  }

  # Change the working directory
  current_wd <- getwd()
  if (change_wd & current_wd != normalizePath(analysis_dir)) {
    rmd_files <- relpath_vec(rmd_files, start = analysis_dir)
    setwd(analysis_dir)
    message(wrap("Current working directory changed to: ", analysis_dir))
  }

  # Open in RStudio
  if (rstudioapi::isAvailable() & open_file) {
    for (rmd in rmd_files) {
      rstudioapi::navigateToFile(rmd)
    }
  }

  return(invisible(rmd_files))
}
