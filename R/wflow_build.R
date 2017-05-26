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
#' \item If \code{republish = TRUE}, all published files will be rebuilt.
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
#' @param republish logical (default: FALSE). Build all published R Markdown
#'   files. Useful for site-wide changes like updating the theme, navigation
#'   bar, or any other setting in \code{_site.yml}.
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
#' @inheritParams wflow_commit
#'
#' @return An object of class \code{wflow_publish}.
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
#' @export
wflow_build <- function(files = NULL, make = is.null(files),
                         update = FALSE, republish = FALSE,
                         seed = 12345, log_dir = NULL,
                         local = FALSE, dry_run = FALSE, project = ".") {

  # Check input arguments ------------------------------------------------------

  if (!is.null(files)) {
    if (!is.character(files)) {
      stop("files must be NULL or a character vector of filenames")
    } else if (!all(file.exists(files))) {
      stop("Not all files exist. Check the paths to the files")
    }
    # Change filepaths to relative paths
    files <- sapply(normalizePath(files), relpath)
  }
  ext <- tools::file_ext(files)
  ext_wrong <- !(ext %in% c("Rmd", "rmd"))
  if (any(ext_wrong)) {
    stop(wrap("File extensions must be either Rmd or rmd."))
  }

  if (!(is.logical(make) && length(make) == 1))
    stop("make must be a one-element logical vector")

  if (!(is.logical(update) && length(update) == 1))
    stop("update must be a one-element logical vector")

  if (!(is.logical(republish) && length(republish) == 1))
    stop("republish must be a one-element logical vector")

  if (!(is.numeric(seed) && length(seed) == 1))
    stop("seed must be a one element numeric vector")

  if (is.null(log_dir)) {
    log_dir <- "/tmp/workflowr"
  } else if (!(is.character(log_dir) && length(log_dir) == 1)) {
    stop("log_dir must be NULL or a one element character vector")
  }
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)

  if (!(is.logical(local) && length(local) == 1))
    stop("local must be a one-element logical vector")

  if (!(is.logical(dry_run) && length(dry_run) == 1))
    stop("dry_run must be a one-element logical vector")

  if (is.character(project) && length(project) == 1) {
    if (dir.exists(project)) {
      project <- normalizePath(project)
    } else {
      stop("project directory does not exist.")
    }
  } else {
    stop("project must be a one-element character vector")
  }

  # Obtain files to consider ---------------------------------------------------

  p <- wflow_paths(project = project)
  # All files to consider
  files_all <- list.files(path = p$analysis, pattern = "^.*\\.[Rr]md$",
                          full.names = TRUE)
  files_all <- sapply(normalizePath(files_all), relpath)
  if (!all(files %in% files_all))
    stop(wrap(
      "Only files in the analysis directory can be built with wflow_build. Use
      `rmarkdown::render` to build non-workflowr files."))

  # Determine which files to build ---------------------------------------------

  files_to_build <- files
  if (make) {
    files_make <- return_modified_rmd(files_all)
    files_to_build <- union(files_to_build, files_make)
  }

  if (update || republish) {
    s <- wflow_status(project = project)
    if (update) {
      files_update <- rownames(s$status)[s$status$mod_committed &
                                        !s$status$mod_unstaged &
                                        !s$status$mod_staged]
      files_to_build <- union(files_to_build, files_update)
    }
    if (republish) {
      files_republish <- rownames(s$status)[s$status$published]
      files_to_build <- union(files_to_build, files_republish)
    }
  }

  # Build files ----------------------------------------------------------------

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

  # Prepare output -------------------------------------------------------------

  built <- to_html(files_to_build, outdir = p$docs)
  o <- list(files = files, make = make,
            update = update, republish = republish,
            seed = seed, log_dir = log_dir,
            local = local, dry_run = dry_run,
            built = built)
  class(o) <- "wflow_build"

  return(o)
}

#' @export
print.wflow_build <- function(x, ...) {
  cat("wflow_build\n\n")
  cat("Settings:\n")
  cat("seed:", x$seed)
  if (x$make) cat(", make: TRUE")
  if (x$update) cat(", update: TRUE")
  if (x$republish) cat(", republish: TRUE")
  cat("\n\n")
  if (x$dry_run & x$local) {
    cat(wrap("The following would be built locally in the current R session:"),
        "\n\n")
  } else if (!x$dry_run & x$local) {
    cat(wrap("The following were built locally in the current R session:"),
        "\n\n")
  } else if (x$dry_run & !x$local) {
    cat(wrap("The following would be built externally each in their own fresh R session:"),
        "\n\n")
  } else if (!x$dry_run & !x$local) {
    cat(wrap("The following were built externally each in their own fresh R session:"),
        "\n\n")
  }
  cat(x$built, sep = "\n")
  if (!x$dry_run & !x$local) {
    cat("\n")
    cat(wrap(sprintf("Log files saved in %s", x$log_dir)))
  }

  return(invisible(x))
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
