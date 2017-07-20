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
#' \item \bold{Scr}: Scratch file. Any untracked file that is not specifically
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
#' \item \bold{root}: The relative path to the root directory of the workflowr
#' project (i.e. contains the RStudio .Rproj file).
#'
#' \item \bold{analysis}: The relative path to the directory that contains
#' \code{_site.yml} and the R Markdown files.
#'
#' \item \bold{docs}: The relative path to the directory that contains the HTML
#' files and figures.
#'
#' \item \bold{git}: The relative path to the \code{.git} directory that
#' contains the history of the Git repository.
#'
#' \item \bold{status}: A data frame with detailed information on the status of
#' each file (see below).
#'
#' }
#'
#' The data frame \code{status} contains the following non-mutually exclusive
#' columns (all logical vectors):
#'
#' \itemize{
#'
#' \item \bold{ignored}: The R Markdown file has been ignored by Git according
#' to the patterns in the file \code{.gitignore}.
#'
#' \item \bold{mod_unstaged}: The R Markdown file has unstaged modifications.
#'
#' \item \bold{mod_staged}: The R Markdown file has staged modifications.
#'
#' \item \bold{tracked}: The R Markdown file is tracked by Git.
#'
#' \item \bold{committed}: The R Markdown file has been previously committed to
#' the Git repository.
#'
#' \item \bold{published}: The corresponding HTML file has been previously
#' committed.
#'
#' \item \bold{mod_committed}: The R Markdown file has modifications that have
#' been committed since the last time the HTML was built and committed.
#'
#' \item \bold{modified}: The R Markdown file has been modified since it was
#' last published (i.e. \code{mod_unstaged} or \code{mod_staged} or
#' \code{mod_committed}).
#'
#' \item \bold{unpublished}: The R Markdown file is tracked by Git but not
#' published (i.e. the HTML has not been committed).
#'
#' \item \bold{scratch}: The R Markdown file is untracked by Git, i.e. it is
#' considered a scratch file until it is committed.
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

  # Ensure Windows paths use forward slashes
  files <- convert_windows_paths(files)
  project <- convert_windows_paths(project)

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
    files <- relpath_vec(files)
    files_analysis <- files_analysis[match(files, files_analysis)]
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
  s <- lapply(s, relpath_vec)
  # Determine status of each analysis file in the Git repository. Each status
  # is a logical vector.
  ignored <- files_analysis %in% s$ignored
  mod_unstaged <- files_analysis %in% s$unstaged
  mod_staged <- files_analysis %in% s$staged
  tracked <- files_analysis %in% setdiff(files_analysis,
                                         c(s$untracked, s$ignored))
  files_committed <- paste0(git2r::workdir(r), get_committed_files(r))
  files_committed <- relpath_vec(files_committed)
  committed <- files_analysis %in% files_committed
  files_html <- to_html(files_analysis, outdir = o$docs)
  published <- files_html %in% files_committed
  # Do published files have subsequently committed changes?
  files_outdated <- get_outdated_files(r,
                                       normalizePath(files_analysis[published]),
                                       outdir = normalizePath(o$docs))
  files_outdated <- relpath_vec(files_outdated)
  mod_committed <- files_analysis %in% files_outdated

  # Highlevel designations
  modified <- published & (mod_unstaged | mod_staged | mod_committed)
  # Status Unp
  #
  # Unpublished file. Any tracked file whose corresponding HTML is not tracked.
  # May or may not have staged or unstaged changes.
  unpublished <- tracked & !published
  # Status Scr
  #
  # Scratch file. Any untracked file that is not specifically ignored.
  scratch <- !tracked & !ignored

  o$status <- data.frame(ignored, mod_unstaged, mod_staged, tracked,
                         committed, published, mod_committed, modified,
                         unpublished, scratch,
                         row.names = files_analysis)

  class(o) <- "wflow_status"
  return(o)
}

#' @export
print.wflow_status <- function(x, ...) {

  # The legend key to explain abbreviations of file status
  key <- character()

  # Report totals
  cat(sprintf("Status of %d files\n\nTotals:\n", nrow(x$status)))
  if (sum(x$status$published) > 0 & sum(x$status$modified) > 0) {
    cat(sprintf(" %d Published (%d Modified)\n",
                sum(x$status$published), sum(x$status$modified)))
    key <- c(key, "Mod = Modified")
  } else if (sum(x$status$published) > 0) {
    cat(sprintf(" %d Published\n", sum(x$status$published)))
  }
  if (sum(x$status$unpublished) > 0) {
    cat(sprintf(" %d Unpublished\n", sum(x$status$unpublished)))
    key <- c(key, "Unp = Unpublished")
  }
  if (sum(x$status$scratch) > 0) {
    cat(sprintf(" %d Scratch\n", sum(x$status$scratch)))
    key <- c(key, "Scr = Scratch (Untracked)")
  }

  f <- c(rownames(x$status)[x$status$modified],
         rownames(x$status)[x$status$unpublished],
         rownames(x$status)[x$status$scratch])
  names(f) <- rep(c("Mod", "Unp", "Scr"),
                  times = c(sum(x$status$modified),
                            sum(x$status$unpublished),
                            sum(x$status$scratch)))

  if (length(f) > 0) {
    cat("\nThe following files require attention:\n\n")
  }
  for (i in seq_along(f)) {
    o <- sprintf("%s %s\n", names(f)[i], f[i])
    cat(o)
  }
  if (length(f) == 0) {
    cat("\nFiles are up-to-date")
  } else {
    m <- sprintf("Key: %s

To publish your changes as part of your website, use `wflow_publish()`.

To commit your changes without publishing them yet, use `wflow_commit()`.",
    paste(key, collapse = ", "))
    cat("\n")
    cat(wrap(m))
  }
  cat("\n")

  # It's a convention for S3 print methods to invisibly return the original
  # object, e.g. base::print.summaryDefault and stats:::print.lm. I don't
  # understand why this is useful. Anyone know why?
  return(invisible(x))
}

