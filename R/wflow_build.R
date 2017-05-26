#' Build the site
#'
#' \code{wflow_build} builds the website from the files in the analysis
#' directory. This is intended to be used when developing your code to preview
#' the changes. When you are ready to commit the files, use
#' \code{\link{wflow_publish}}.
#'
#' \code{wflow_build} has multiple, non-mutually exclusive options for deciding
#' which files to build:
#'
#' \itemize{
#'
#' \item Files specified via the argument \code{files} are always built.
#'
#' \item If \code{make = TRUE}, all files which have been modified more recently
#' than their corresponding HTML files will be built.
#'
#' \item If \code{update = TRUE}, all files which have been committed more
#' recently than their corresponding HTML files will be built. However, files
#' which currently have staged or unstaged changes will be ignored.
#'
#' \item If \code{republish = TRUE}, all files will be built.
#'
#' }
#'
#' Under the hood, \code{wflow_build} is a wrapper for
#' \code{\link[rmarkdown]{render_site}} from the package \link{rmarkdown}.
#'
#' @param files character (default: NULL). Files to build. Supports file
#'   extensions Rmd, rmd, and md. Only files in the analysis directory are
#'   allowed (and therefore any path to a file is ignored).
#' @param make logical (default: \code{is.null(files)}). Use Make-like behavior,
#'   i.e. build the files that have been updated more recently than their
#'   corresponding HTML files. This is the default action if no files are
#'   specified.
#' @param update logical (default: FALSE). Build the files that have been
#'   committed more recently than their corresponding HTML files (and do not
#'   have any unstaged or staged changes). This ensures that the commit version
#'   ID inserted into the HTML corresponds to the exact version of the source
#'   file that was used to produce it.
#' @param republish logical (default: FALSE). Build all R Markdown (and
#'   Markdown) files. Useful for site-wide changes like updating the theme,
#'   navigation bar, or any other setting in \code{_site.yml}.
#' @param seed numeric (default: 12345). The seed to set before building each
#'   file. Passed to \code{\link{set.seed}}.
#' @param log_dir character (default: NULL). The directory to save log files
#'   from building files. It will be created if necessary and ignored if
#'   \code{local = TRUE}. The default is to create a directory in \code{/tmp}.
#' @param local logical (default: FALSE). Build files locally in the R console.
#'   This should only be used for debugging purposes. The default is to build
#'   each file in its own separate fresh R process to ensure each file is
#'   reproducible in isolation.
#' @param dry_run logical (default: FALSE). Preview the files to be built, but
#'   do not actually build them.
#' @inheritParams wflow_commit_
#'
#' @return A character vector of the built files
#'
#' @seealso \code{\link{wflow_publish}}
#'
#' @examples
#' \dontrun{
#'
#' # Build all files
#' wflow_build() # equivalent to wflow_build(make = TRUE)
#' # Build a single file
#' wflow_build("file.Rmd")
#' # Build multiple files
#' wflow_build(c("file1.Rmd", "file2.Rmd"))
#' # Build every file
#' wflow_build(republish = TRUE)
#' }
#'
#' @import rmarkdown
#'
wflow_build_ <- function(files = NULL, make = is.null(files),
                         update = FALSE, republish = FALSE,
                         seed = 12345, log_dir = NULL,
                         local = FALSE, dry_run = FALSE, project = ".") {
  if (!(is.null(files) | is.character(files)))
    stop("files must be NULL or a character vector")
  if (!is.logical(make) | length(make) != 1)
    stop("make must be a one-element logical vector")
  if (!is.logical(update) | length(update) != 1)
    stop("update must be a one-element logical vector")
  if (!is.logical(republish) | length(republish) != 1)
    stop("republish must be a one-element logical vector")
  if (!is.numeric(seed) | length(seed) != 1)
    stop("seed must be a one element numeric vector")
  if (!(is.null(log_dir) | (is.character(log_dir) & length(log_dir) == 1)))
    stop("log_dir must be a one element character vector")
  if (!is.logical(local) | length(local) != 1)
    stop("local must be a one-element logical vector")
  if (!is.logical(dry_run) | length(dry_run) != 1)
    stop("dry_run must be a one-element logical vector")
  if (!is.character(project) | length(project) != 1)
    stop("project must be a one element character vector")

  # Check that directories and files exist
  if (!dir.exists(project)) {
    stop("project does not exist.")
  } else {
    project <- normalizePath(project)
  }
  if (is.null(log_dir))
    log_dir <- "/tmp/workflowr"
  if (!is.null(files)) {
    files_missing <- !file.exists(files)
    if (any(files_missing)) {
      stop("missing files: ", files[files_missing])
    } else {
      files <- normalizePath(files)
    }
  }
  files_to_build <- files

  root_path <- rprojroot::find_rstudio_root_file(path = project)
  analysis_dir <- file.path(root_path, "analysis")

  files_all <- Sys.glob(file.path(analysis_dir, "*Rmd"))

  if (make) {
    files_make <- return_modified_rmd(files_all)
    files_to_build <- union(files_to_build, files_make)
  }

  # This currently gets every Rmd file. May want to change to only tracked files
  if (republish) {
    files_to_build <- files_all
  } else if (update) {
    # Build files if their corresponding HTML file in out-of-date in the Git
    # commit history
    #
    # To do: Adapt from wflow_commit
  }

  if (!dry_run) {
    for (f in files_to_build) {
      cat(sprintf("\n\nRendering %s\n\n", f))
      if (local) {
        build_rmd(f, seed = seed, envir = new.env())
      } else {
        build_rmd_external(f, seed = seed, log_dir = log_dir)
      }
    }
  }

  return(invisible(files_to_build))
}

#' Build the website
#'
#' \code{wflow_build} builds the website by rendering the R Markdown files in
#' the analysis directory and saving them to \code{docs/}. By default it only
#' renders the R Markdown files that have been modified more recently than their
#' corresponding HTML file (similar to a Makefile). To render every single page
#' (e.g. to change the theme across the entire site), set \code{all = TRUE}. To
#' render specific R Markdown files, pass them as a vector to the argument
#' \code{files}.
#'
#' Under the hood, this runs \code{rmarkdown::render_site} on each updated file
#' individually. This provides all the website styling specified in
#' \code{_site.yml} (which you would lose if you ran \code{rmarkdown::render})
#' without re-building the entire site (which would happen if you ran
#' \code{rmarkdown::render_site with no arguments}).
#'
#' To include R Markdown files in your workflowr project that are not included
#' as part of the website, you have multiple options: 1) Prepend an underscore
#' to the filename, 2) move them to a subdirectory within the analysis
#' directory, or 3) move them to another directory at the root of your project.
#'
#' @param all logical indicating if every R Markdown file should be rendered
#'   when building the site (default: FALSE).
#' @param files R Markdown files to be rendered. The files can be specified
#'   using the path or just the basename (default: NULL).
#' @param dry_run Identifies R Markdown files that have been updated, but does
#'   not render them.
#' @param path By default the function assumes the current working directory is
#'   within the project. If this is not true, you'll need to provide the path to
#'   the project directory.
#' @param ... Additional arguments that can be passed to
#'   \code{rmarkdown::render_site}. Should only be needed for testing potential
#'   changes. Any permanent settings should be specified in
#'   \code{analysis/_site.yml}.
#'
#' @return If \code{dry_run = TRUE}, returns the character vector of R Markdown
#'   files that would be rendered. Otherwise invisibly returns this vector.
#'
#' @examples
#' \dontrun{
#' # View the files that would be rendered
#' wflow_build(dry_run = TRUE)
#' # Render all modified files
#' wflow_build()
#' # Render all files
#' wflow_build(all = TRUE)
#' # Render specific files
#' wflow_build(files = c("one.Rmd", "two.Rmd"))
#' }
#' @import rmarkdown
#' @export
wflow_build <- function(all = FALSE, files = NULL, dry_run = FALSE,
                        path = ".", ...) {
  stopifnot(is.logical(all),
            is.null(files) | is.character(files),
            is.logical(dry_run),
            is.character(path))
  analysis_dir <- rprojroot::find_rstudio_root_file("analysis", path = path)
  stopifnot(dir.exists(analysis_dir))

  if (all | is.null(files)) {
    # Gather Rmd files (any file starting with _ is ignored)
    rmd_files <- list.files(path = analysis_dir, pattern = "^[^_].*Rmd$",
                            full.names = TRUE)
  } else {
    rmd_files <- file.path(analysis_dir, basename(files))
    stopifnot(file.exists(rmd_files), grepl("Rmd$", rmd_files))
  }

  if (all | !is.null(files)) {
    files_to_update <- rmd_files
  } else {
    files_to_update <- return_modified_rmd(rmd_files)
  }

  # Render the updated R Markdown files
  if (length(files_to_update) == 0) {
    message("All HTML files have been rendered")
  } else if (dry_run) {
    message("The following R Markdown files would be rendered:")
    return(files_to_update)
  } else {
    for (f in files_to_update) {
      cat(sprintf("\n\nRendering %s\n\n", f))
      rmarkdown::render_site(f, envir = new.env(), ...)
    }
  }

  return(invisible(files_to_update))
}

# Return the R Markdown files which have been modified more recently than their
# corresponding HTML files in docs/.
#
# Input: character. path to R Markdown files in analysis directory
return_modified_rmd <- function(rmd_files) {

  # Expected html files
  html_files <- stringr::str_replace(rmd_files, "Rmd$", "html")
  html_files <- stringr::str_replace(html_files, "/analysis/", "/docs/")

  # Determine which R Markdown files have been updated and need to be rendered
  files_to_update <- character()
  for (i in seq_along(rmd_files)) {
    rmd_timestamp <- file.mtime(rmd_files[i])
    html_timestamp <- file.mtime(html_files[i])
    if (is.na(html_timestamp)) {
      files_to_update <- c(files_to_update, rmd_files[i])
    } else if (rmd_timestamp > html_timestamp) {
      files_to_update <- c(files_to_update, rmd_files[i])
    }
  }

  return(files_to_update)
}

build_rmd_external <- function(rmd, seed, log_dir) {
  if (!is.character(rmd) | length(rmd) != 1)
    stop("rmd must be a one element character vector")
  if (!file.exists(rmd))
    stop("rmd does not exist: ", rmd)
  if (!is.numeric(seed) | length(seed) != 1)
    stop("seed must be a one element numeric vector")
  if (!is.character(log_dir) | length(log_dir) != 1)
    stop("log_dir must be a one element character vector")
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
    message("log directory created: ", log_dir)
  }

  rmd_base <- basename(rmd)
  date_current <- format(Sys.time(), "%Y-%m-%d-%Hh-%Mm-%Ss")
  stdout_file <- file.path(log_dir,
                           paste(rmd_base, date_current, "out.txt", sep = "-"))
  stderr_file <- file.path(log_dir,
                           paste(rmd_base, date_current, "err.txt", sep = "-"))
  result <- tryCatch(
    callr::r_safe(build_rmd,
                  args = list(rmd, seed),
                  stdout = stdout_file,
                  stderr = stderr_file),
    error = function(e) return(FALSE)
  )
  return(result)
}

build_rmd <- function(rmd, seed, ...) {
  if (!is.character(rmd) || length(rmd) != 1)
    stop("rmd must be a one element character vector")
  if (!is.numeric(seed) | length(seed) != 1)
    stop("seed must be a one element numeric vector")
  if (!file.exists(rmd))
    stop("rmd must exist")

  set.seed(seed)
  rmarkdown::render_site(rmd, ...)
}
