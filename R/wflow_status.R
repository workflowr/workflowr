#' Report status of workflowr project.
#'
#' \code{wflow_status} reports the analysis files that require user
#' action.
#'
#' \code{wflow_status} reports analysis files with one of the
#' following statuses:
#'
#' \itemize{
#'
#' \item \bold{Mod}: Modified file. Any published file that has been
#' modified since the last time the HTML was published.
#'
#' \item \bold{Unp}: Unpublished file. Any tracked file whose
#' corresponding HTML is not tracked. May or may not have staged or
#' unstaged changes.
#'
#' \item \bold{New}: New file. Any untracked file that is not
#' specifically ignored.
#'
#' }
#'
#'@param project character (default: ".") By default the function
#'  assumes the current working directory is within the project. If
#'  this is not true, you'll need to provide the path to the project
#'  directory.
#'
#'@return Returns an object of class \code{wflow_status}, which is a
#'  list with the following elements:
#'
#'  \itemize{
#'
#'  \item \bold{wd}: The current working directory in the R console
#'  (i.e. \code{getwd{}}).
#'
#'  \item \bold{root}: The root directory of the workflowr project
#'  (i.e. contains the RStudio .Rproj file).
#'
#'  \item \bold{analysis}: The directory that contains
#'  \code{_site.yml} and the R Markdown files.
#'
#'  \item \bold{docs}: The directory that contains the HTML files and
#'  figures.
#'
#'  \item \bold{files}: The files whose status was checked.
#'
#'  \item \bold{git}: The \code{.git} directory that contains the
#'  history of the Git repository.
#'
#'  \item \bold{git_status}: The output from
#'  \code{git2r::\link[git2r]{status}}.
#'
#'  \item \bold{status}: A data frame with detailed information on the
#'  status of each file (see below).
#'
#'  }
#'
#'  The data frame \code{status} contains the following columns (all
#'  logical vectors):
#'
#'  \itemize{
#'
#'  \item \bold{outdated}: When an R Markdown file has been committed
#'  to the repository without updating the previously published HTML
#'  file.
#'
#'  \item \bold{staged}: When an R Markdown file has changes that have
#'  been added to the index (e.g. with \code{git add}).
#'
#'  \item \bold{unstaged}: When a tracked R Markdown file has changes
#'  in the working directory.
#'
#'  \item \bold{untracked}: When an R Markdown file has not been added
#'  or committed to the Git repository.
#'
#'  \item \bold{ignored}: When an R Markdown file has been ignored by
#'  Git according to the patterns in the file \code{.gitignore}.
#'
#'  }
#'
#' @examples
#' \dontrun{
#'
#' wflow_status()
#' # Save the results
#' s <- wflow_status()
#' }
#'@export
wflow_status <- function(project = ".") {
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
  files_analysis <- files_all[files_all_ext %in% c("Rmd", "rmd")]
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
    if (dir.exists(o$docs)) {
      html <- to_html(files_analysis, outdir = o$docs)
      # Has the HTML file been built?
      built <- file.exists(html)
      # Has the HTML file been committed?
      committed <- paste0(git2r::workdir(r), get_committed_files(r))
      published <- html %in% committed
      # Is the committed HTML file up-to-date?
      files_outdated <- get_outdated_files(r, files_analysis[published],
                                           outdir = o$docs)
      up_to_date <- published & !(files_analysis %in% files_outdated)
      o$status <- cbind(o$status, built, published, up_to_date)
    }
  }

  return(o)
}

#' @export
print.wflow_status <- function(x, ...) {
  # Status Mod
  #
  # Published file that has been modified since last publication
  modified <- x$status$published & (x$status$staged |
                                      x$status$unstaged |
                                      !x$status$up_to_date)
  # Status Unp
  #
  # Unpublished file. Any tracked file whose corresponding HTML is not
  # tracked. May or may not have staged or unstaged changes.
  unpublished <- x$status$tracked & !x$status$published
  # Status New
  #
  # New file. Any untracked file that is not specifically ignored.
  new <- !x$status$tracked & !x$status$ignored

  for (i in seq_along(x$files)) {
    file_rel <- relpath(x$files[i], start = getwd())
    if (modified[i]) {
      o <- sprintf("Mod %s\n", file_rel)
      cat(o)
    } else if (unpublished[i]) {
      o <- sprintf("Unp %s\n", file_rel)
      cat(o)
    } else if (new[i]) {
      o <- sprintf("New %s\n", file_rel)
      cat(o)
    }
  }
}
