#' Report status of workflowr project.
#'
#' \code{wflow_status} reports the analysis files that require user action.
#'
#' \code{wflow_status} reports analysis files with one of the following
#' statuses:
#'
#' \itemize{
#'
#' \item \bold{Mod}: Modified file. Any published file that has been modified
#' since the last time the HTML was published.
#'
#' \item \bold{Unp}: Unpublished file. Any tracked file whose corresponding HTML
#' is not tracked. May or may not have staged or unstaged changes.
#'
#' \item \bold{New}: New file. Any untracked file that is not specifically
#' ignored.
#'
#' }
#'
#' \code{wflow_status} only works for workflowr projects that use Git.
#'
#' @param files character (default: NULL) The analysis file(s) to report the
#'   status. By default checks the status of all analysis files.
#' @param project character (default: ".") By default the function assumes the
#'   current working directory is within the project. If this is not true,
#'   you'll need to provide the path to the project directory.
#'
#' @return Returns an object of class \code{wflow_status}, which is a list with
#'   the following elements:
#'
#' \itemize{
#'
#' \item \bold{wd}: The current working directory in the R console (i.e.
#' \code{getwd{}}).
#'
#' \item \bold{root}: The root directory of the workflowr project (i.e. contains
#' the RStudio .Rproj file).
#'
#' \item \bold{analysis}: The directory that contains \code{_site.yml} and the R
#' Markdown files.
#'
#' \item \bold{docs}: The directory that contains the HTML files and figures.
#'
#' \item \bold{files}: The files whose status was checked.
#'
#' \item \bold{git}: The \code{.git} directory that contains the history of the
#' Git repository.
#'
#' \item \bold{git_status}: The output from \code{git2r::\link[git2r]{status}}.
#'
#' \item \bold{status}: A data frame with detailed information on the status of
#' each file (see below).
#'
#' }
#'
#' The data frame \code{status} contains the following columns (all logical
#' vectors):
#'
#' \itemize{
#'
#' \item \bold{outdated}: When an R Markdown file has been committed to the
#' repository without updating the previously published HTML file.
#'
#' \item \bold{staged}: When an R Markdown file has changes that have been added
#' to the index (e.g. with \code{git add}).
#'
#' \item \bold{unstaged}: When a tracked R Markdown file has changes in the
#' working directory.
#'
#' \item \bold{untracked}: When an R Markdown file has not been added or
#' committed to the Git repository.
#'
#' \item \bold{ignored}: When an R Markdown file has been ignored by Git
#' according to the patterns in the file \code{.gitignore}.
#'
#' }
#'
#' @examples
#' \dontrun{
#'
#' wflow_status()
#' # Get status of specific file(s)
#' wflow_status("analysis/file.Rmd")
#' # Save the results
#' s <- wflow_status()
#' }
#' @export
wflow_status <- function(files = NULL, project = ".") {
  if (!(is.null(files) | is.character(files)))
      stop("files must be NULL or a character vector")
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
  if (!is.null(files)) {
    files <- normalizePath(files, mustWork = FALSE)
    files_analysis <- files_analysis[files_analysis %in% files]
  }
  o$files <- files_analysis
  if (length(o$files) == 0)
    stop("files did not include any analysis files")

  # Git repository
  r <- try(git2r::repository(o$root, discover = TRUE), silent = TRUE)
  if (class(r) == "try-error") {
    stop("wflow_status only works if the workflowr project uses Git.")
  } else {
    o$git <- normalizePath(r@path) # remove trailing slash
  }

  # Obtain status of each file
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
  # Unpublished file. Any tracked file whose corresponding HTML is not tracked.
  # May or may not have staged or unstaged changes.
  unpublished <- x$status$tracked & !x$status$published
  # Status New
  #
  # New file. Any untracked file that is not specifically ignored.
  new <- !x$status$tracked & !x$status$ignored

  # The legend key to explain abbreviations of file status
  key <- character()

  # Report totals
  message(sprintf("Status of %d files\n\nTotals:", nrow(x$status)))
  if (sum(x$status$published) > 0 & sum(modified) > 0) {
    message(sprintf(" %d Published (%d Modified)",
            sum(x$status$published), sum(modified)))
    key <- c(key, "Mod = Modified")
  } else if (sum(x$status$published) > 0) {
    message(sprintf(" %d Published", sum(x$status$published)))
  }
  if (sum(unpublished) > 0) {
    message(sprintf(" %d Unpublished", sum(unpublished)))
    key <- c(key, "Unp = Unpublished")
  }
  if (sum(new) > 0) {
    message(sprintf(" %d New", sum(new)))
    key <- c(key, "New = Untracked")
  }

  f <- c(x$files[modified],x$files[unpublished], x$files[new])
  names(f) <- rep(c("Mod", "Unp", "New"),
                  times = c(sum(modified), sum(unpublished), sum(new)))

  if (length(f) > 0) {
    message("\nThe following files require attention:")
  }
  for (i in seq_along(f)) {
    f_rel <- relpath(f[i], start = getwd())
    o <- sprintf("%s %s\n", names(f)[i], f_rel)
    cat(o)
  }
  if (length(f) == 0) {
    message("\nFiles are up-to-date")
  } else {
    m <- sprintf("Key: %s

To publish your changes as part of your website, use `wflow_publish()`.

To commit your changes without publishing them yet, use `wflow_commit()`.",
    paste(key, collapse = ", "))
    message(wrap(m))
  }

  # It's a convention for S3 print methods to invisibly return the original
  # object, e.g. base::print.summaryDefault and stats:::print.lm. I don't
  # understand why this is useful. Anyone know why?
  return(invisible(x))
}
