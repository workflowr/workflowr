#' Report status of workflowr project
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
#'   status. By default checks the status of all analysis files. Supports
#'   file \href{https://en.wikipedia.org/wiki/Glob_(programming)}{globbing}.
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
#' \item \bold{site_yml}: \code{TRUE} if the configuration file \code{_site.yml}
#' has uncommitted changes, otherwise \code{FALSE}.
#'
#' \item \bold{wflow_yml}: \code{TRUE} if the configuration file
#' \code{_workflowr.yml} has uncommitted changes, otherwise \code{FALSE}. If the
#' file does not exist, the result is \code{NULL}. If the file was recently
#' deleted and not yet committed to Git, then it will be \code{TRUE}.
#'
#' \item \bold{status}: A data frame with detailed information on the status of
#' each R Markdown file (see below).
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
#' \item \bold{conflicted}: The R Markdown file has merge conflicts.
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

  if (!is.null(files)) {
    if (!(is.character(files) && length(files) > 0))
      stop("files must be NULL or a character vector of filenames")
    if (any(fs::dir_exists(files)))
      stop("files cannot include a path to a directory")
    files <- glob(files)
    if (!all(fs::file_exists(files)))
      stop("Not all files exist. Check the paths to the files")
    # Change filepaths to relative paths
    files <- relative(files)
    # Check for valid file extensions
    ext <- tools::file_ext(files)
    ext_wrong <- !(ext %in% c("Rmd", "rmd"))
    if (any(ext_wrong))
      stop(wrap("File extensions must be either Rmd or rmd."))
  }

  if (!(is.character(project) && length(project) == 1))
    stop("project must be a one element character vector")
  if (!fs::dir_exists(project))
    stop("project does not exist.")
  project <- absolute(project)

  # Obtain list of workflowr paths. Throw error if no Git repository.
  o <- wflow_paths(error_git = TRUE, project = project)

  # Gather analysis files
  # (files that start with an underscore are ignored)
  files_analysis <- list.files(path = o$analysis, pattern = "^[^_].+[Rr]md$",
                          full.names = TRUE)
  files_analysis <- relative(files_analysis)

  if (!is.null(files)) {
    files_analysis <- files_analysis[match(files, files_analysis)]
  }
  if (length(files_analysis) == 0)
    stop("files did not include any analysis files")

  # Obtain status of each R Markdown file
  r <- git2r::repository(o$git)
  s <- git2r::status(r, ignored = TRUE)
  s_df <- status_to_df(s)
  # Fix file paths
  s_df$file <- file.path(git2r_workdir(r), s_df$file)
  s_df$file <- relative(s_df$file)
  # Categorize all files by git status
  f_ignored <- s_df$file[s_df$status == "ignored"]
  f_unstaged <- s_df$file[s_df$status == "unstaged"]
  f_conflicted <- s_df$file[s_df$substatus == "conflicted"]
  f_staged <- s_df$file[s_df$status == "staged"]
  f_untracked <- s_df$file[s_df$status == "untracked"]
  # Determine status of each analysis file (i.e. Rmd) in the Git repository.
  # Each status is a logical vector.
  ignored <- files_analysis %in% f_ignored
  mod_unstaged <- files_analysis %in% f_unstaged
  conflicted <- files_analysis %in% f_conflicted
  mod_staged <- files_analysis %in% f_staged
  tracked <- files_analysis %in% setdiff(files_analysis,
                                         c(f_untracked, f_ignored))
  files_committed <- get_committed_files(r)
  files_committed <- relative(files_committed)
  committed <- files_analysis %in% files_committed
  files_html <- to_html(files_analysis, outdir = o$docs)
  published <- files_html %in% files_committed
  # Do published files have subsequently committed changes?
  files_outdated <- get_outdated_files(r,
                                       absolute(files_analysis[published]),
                                       outdir = absolute(o$docs))
  files_outdated <- relative(files_outdated)
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

  # Determine if _site.yml has been edited
  o$site_yml <- FALSE
  site_yml_path <- relative(file.path(o$analysis, "_site.yml"))
  if (site_yml_path %in% s_df$file) o$site_yml <- TRUE

  # Determine if _workflowr.yml has been edited
  o$wflow_yml <- FALSE
  wflow_yml_path <- relative(file.path(o$root, "_workflowr.yml"))
  if (!file.exists(wflow_yml_path)) o$wflow_yml <- NULL
  if (wflow_yml_path %in% s_df$file) o$wflow_yml <- TRUE

  o$status <- data.frame(ignored, mod_unstaged, conflicted, mod_staged, tracked,
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

To commit your changes without publishing them yet, use `wflow_git_commit()`.",
    paste(key, collapse = ", "))
    cat("\n")
    cat(wrap(m))
  }
  cat("\n")

  if (x$site_yml) {
    site_yml_path <- relative(file.path(x$analysis, "_site.yml"))
    cat(glue::glue("\n\nThe config file {site_yml_path} has been edited.\n\n"))
  }

  if (!is.null(x$wflow_yml) && x$wflow_yml) {
    wflow_yml_path <- relative(file.path(x$root, "_workflowr.yml"))
    cat(glue::glue("\n\nThe config file {wflow_yml_path} has been edited.\n\n"))
  }

  # It's a convention for S3 print methods to invisibly return the original
  # object, e.g. base::print.summaryDefault and stats:::print.lm. I don't
  # understand why this is useful. Anyone know why?
  return(invisible(x))
}

