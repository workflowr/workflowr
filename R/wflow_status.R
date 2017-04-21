#' Report status of workflowr project.
#'
#' \code{wflow_status} reports the current status of the workflowr project.
#'
#' @param verbose logical (default: TRUE). Prints status to R console.
#' @param project character (default: ".") By default the function assumes the
#'   current working directory is within the project. If this is not true,
#'   you'll need to provide the path to the project directory.
#'
#' @return Invisibly returns an object of class \code{wflow_status}.
#'
#' @examples
#' \dontrun{
#'
#' wflow_status()
#' # Save the results
#' s <- wflow_status()
#' }
#' @export
wflow_status <- function(verbose = TRUE, project = ".") {
  if (!is.logical(verbose) | length(verbose) != 1)
    stop("verbose must be a one-element logical vector")
  if (!is.character(project) | length(project) != 1)
    stop("project must be a one element character vector")
  if (!dir.exists(project))
    stop("project does not exist.")

  # Create list to store output
  o <- list()
  class(o) <- "wflow_status"

  # Working directory
  o$wd <- getwd()

  # workflowr root
  project <- normalizePath(project)
  o$root <- try(rprojroot::find_rstudio_root_file(path = project),
                silent = TRUE)
  if (class(o$root) == "try-error")
    stop(wrap(
      "Unable to find RStudio Rproj file at the root of the workflowr project.
      Did you delete it?"),
      call. = FALSE)

  # Analysis directory with _site.yml
  top_level_files <- list.files(path = o$root, full.names = TRUE)
  subdirs <- top_level_files[dir.exists(top_level_files)]
  site_file <- list.files(path = subdirs, pattern = "_site.yml",
                          full.names = TRUE)
  if (length(site_file) == 0) {
    stop("Unable to find the file _site.yml in the analysis directory. Is this a workflowr project?", call. = FALSE)
  } else if (length(site_file) > 1) {
    stop("Found more than one _site.yml file. Only one subdirectory at the top level of the workflowr project can contain _site.yml.", call. = FALSE)
  } else {
    o$analysis <- dirname(site_file)
  }

  # docs/ directory
  output_dir <- yaml::yaml.load_file(site_file)$output_dir
  if (is.null(output_dir))
    stop("Unable to locate the website directory. Make sure to set the variable output_dir in the file _site.yml", call. = FALSE)
  o$docs <- normalizePath(file.path(o$analysis, output_dir), mustWork = FALSE)
  if (!dir.exists(o$docs)) {
    o$docs <- NA
    warning("Unable to locate docs directory. Run wflow_build() to create it.")
  }

  # Gather analysis files
  # (files that start with an underscore are ignored)
  files_all <- list.files(path = o$analysis, pattern = "^[^_]", full.names = TRUE)
  files_all_ext <- tools::file_ext(files_all)
  files_analysis <- files_all[files_all_ext %in% c("Rmd", "rmd", "md")]
  o$files <- files_analysis

  # Git repository
  r <- try(git2r::repository(o$root, discover = TRUE), silent = TRUE)
  if (class(r) == "try-error") {
    o$git <- NA
  } else {
    o$git <- normalizePath(r@path) # remove trailing slash
  }

  # Obtain status of each file
  if (!is.na(o$git)) {
    s <- git2r::status(r, ignored = TRUE)
    o$git_status <- s
    # Convert from a list of lists of relative paths to a list of character
    # vectors of absolute paths
    s <- lapply(s, function(x) paste0(git2r::workdir(r), as.character(x)))
    # Determine status of each analysis file in the Git repository. Each status
    # is a logical vector.
    tracked <- files_analysis %in% setdiff(files_analysis,
                                           c(s$untracked, s$ignored))
    staged <- files_analysis %in% s$staged
    unstaged <- files_analysis %in% s$unstaged
    ignored <- files_analysis %in% s$ignored
    o$status <- data.frame(tracked, staged, unstaged, ignored,
                           row.names = files_analysis)
  }

  return(invisible(o))
}

# @export
print.wflow_status <- function(x) {
  m <- sprintf("workflowr status report\n
               ------------------------\n
               Working directory:\t%s\n
               Root of workflowr project:\t%s\n",
               x$wd, x$root)
  cat(wrap(m))
  # message("this is your status")
  # message("Working directory:", x$wd)
  # message("Root of worflowr project:", x$root)
}
