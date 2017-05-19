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

  # Obtain list of workflowr paths. Throw error if no Git repository.
  o <- wflow_paths(error_git = TRUE, project = project)

  # Gather analysis files
  # (files that start with an underscore are ignored)
  files_all <- list.files(path = o$analysis, pattern = "^[^_]")
  if (o$analysis != ".")
    files_all <- file.path(o$analysis, files_all)
  files_all_ext <- tools::file_ext(files_all)
  files_analysis <- files_all[files_all_ext %in% c("Rmd", "rmd")]

  if (!is.null(files)) {
    # Don't know if file paths are relative or absolute, so ensure they are
    # relative
    files <- normalizePath(files, mustWork = FALSE)
    files <- sapply(files, relpath)
    files_analysis <- files_analysis[files_analysis %in% files]
  }
  if (length(files_analysis) == 0)
    stop("files did not include any analysis files")

  # Obtain status of each file
  r <- git2r::repository(o$git)
  s <- git2r::status(r, ignored = TRUE)
  # Convert from a list of lists of paths relative to the .git directory to a
  # list of character vectors of absolute paths
  s <- lapply(s, function(x) paste0(git2r::workdir(r), as.character(x)))
  # Convert from absolute paths to paths relative to working directory
  s <- lapply(s, function(x) sapply(x, relpath))
  # Determine status of each analysis file in the Git repository. Each status
  # is a logical vector.
  ignored <- files_analysis %in% s$ignored
  mod_unstaged <- files_analysis %in% s$unstaged
  mod_staged <- files_analysis %in% s$staged
  tracked <- files_analysis %in% setdiff(files_analysis,
                                         c(s$untracked, s$ignored))
  files_committed <- paste0(git2r::workdir(r), get_committed_files(r))
  files_committed <- sapply(files_committed, relpath)
  committed <- files_analysis %in% files_committed
  files_html <- to_html(files_analysis, outdir = o$docs)
  published <- files_html %in% files_committed
  # Do published files have subsequently committed changes?
  files_outdated <- get_outdated_files(r,
                                       normalizePath(files_analysis[published]),
                                       outdir = normalizePath(o$docs))
  files_outdated <- sapply(files_outdated, relpath)
  mod_committed <- files_analysis %in% files_outdated

  # Highlevel designations
  modified <- published & (mod_unstaged | mod_staged | mod_committed)
  # Status Unp
  #
  # Unpublished file. Any tracked file whose corresponding HTML is not tracked.
  # May or may not have staged or unstaged changes.
  unpublished <- tracked & !published
  # Status New
  #
  # New file. Any untracked file that is not specifically ignored.
  new <- !tracked & !ignored

  o$status <- data.frame(ignored, mod_unstaged, mod_staged, tracked,
                         committed, published, mod_committed, modified,
                         unpublished, new,
                         row.names = files_analysis)

  class(o) <- "wflow_status"
  return(o)
}

#' @export
print.wflow_status <- function(x, ...) {

  # The legend key to explain abbreviations of file status
  key <- character()

  # Report totals
  message(sprintf("Status of %d files\n\nTotals:", nrow(x$status)))
  if (sum(x$status$published) > 0 & sum(x$status$modified) > 0) {
    message(sprintf(" %d Published (%d Modified)",
            sum(x$status$published), sum(x$status$modified)))
    key <- c(key, "Mod = Modified")
  } else if (sum(x$status$published) > 0) {
    message(sprintf(" %d Published", sum(x$status$published)))
  }
  if (sum(x$status$unpublished) > 0) {
    message(sprintf(" %d Unpublished", sum(x$status$unpublished)))
    key <- c(key, "Unp = Unpublished")
  }
  if (sum(x$status$new) > 0) {
    message(sprintf(" %d New", sum(x$status$new)))
    key <- c(key, "New = Untracked")
  }

  f <- c(rownames(x$status)[x$status$modified],
         rownames(x$status)[x$status$unpublished],
         rownames(x$status)[x$status$new])
  names(f) <- rep(c("Mod", "Unp", "New"),
                  times = c(sum(x$status$modified),
                            sum(x$status$unpublished),
                            sum(x$status$new)))

  if (length(f) > 0) {
    message("\nThe following files require attention:")
  }
  for (i in seq_along(f)) {
    o <- sprintf("%s %s\n", names(f)[i], f[i])
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

