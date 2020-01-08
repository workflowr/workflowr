#' Build the site
#'
#' \code{wflow_build} builds the website from the files in the analysis
#' directory. This is intended to be used when developing your code to preview
#' the changes. When you are ready to commit the files, use
#' \code{\link{wflow_publish}}.
#'
#' \code{wflow_build} has multiple, non-mutually exclusive options for deciding
#' which files to build. In other words, if multiple options are set to
#' \code{TRUE}, the union of all files will be built. The argument \code{make}
#' is the most useful for interactively performing your analysis. The other
#' options are more useful when you are ready to publish specific files with
#' \code{\link{wflow_publish}} (which passes these arguments to
#' \code{wflow_build}).
#'
#' \itemize{
#'
#' \item Files specified via the argument \code{files} are always built.
#'
#' \item If \code{make = TRUE}, all files which have been modified more recently
#' than their corresponding HTML files will be built.
#'
#' \item If \code{update = TRUE}, all previously published files which have been
#' committed more recently than their corresponding HTML files will be built.
#' However, files which currently have staged or unstaged changes will be
#' ignored.
#'
#' \item If \code{republish = TRUE}, all published files will be rebuilt.
#' However, files which currently have staged or unstaged changes will be
#' ignored.
#'
#' }
#'
#' Under the hood, \code{wflow_build} is a wrapper for
#' \code{\link[rmarkdown]{render_site}} from the package \link{rmarkdown}.
#'
#' @param files character (default: NULL). Files to build. Only allows files in
#'   the analysis directory with the extension Rmd or rmd. If \code{files} is
#'   \code{NULL}, the default behavior is to build all outdated files (see
#'   argument \code{make} below). Supports file
#'   \href{https://en.wikipedia.org/wiki/Glob_(programming)}{globbing}.
#'   The files are always built in the order they are listed.
#' @param make logical (default: \code{is.null(files)}). When \code{make =
#'   TRUE}, build any files that have been modified more recently than their
#'   corresponding HTML files (inspired by
#'   \href{https://en.wikipedia.org/wiki/Make_(software)}{Make}). This is the
#'   default action if no files are specified.
#' @param update logical (default: FALSE). Build any files that have been
#'   committed more recently than their corresponding HTML files (and do not
#'   have any unstaged or staged changes). This ensures that the commit version
#'   ID inserted into the HTML corresponds to the exact version of the source
#'   file that was used to produce it.
#' @param republish logical (default: FALSE). Build all published R Markdown
#'   files (that do not have any unstaged or staged changes). Useful for
#'   site-wide changes like updating the theme, navigation bar, or any other
#'   setting in \code{_site.yml}.
#' @param view logical (default: \code{getOption("workflowr.view")}). View the
#'   website with \code{\link{wflow_view}} after building files. If only one
#'   file is built, it is opened. If more than one file is built, the main index
#'   page is opened. Not applicable if no files are built or if \code{dry_run =
#'   TRUE}.
#' @param clean_fig_files logical (default: FALSE). Delete existing figure files
#'   for each R Markdown file prior to building it. This ensures that only
#'   relevant figure files are saved. As you develop an analysis, it is easy to
#'   generate lots of unused plots due to changes in the number of code chunks
#'   and their names. However, if you are caching chunks during code
#'   development, this could cause figures to disappear. Note that
#'   \code{\link{wflow_publish}} uses \code{clean_fig_files = TRUE} to ensure
#'   the results can be reproduced.
#' @param delete_cache logical (default: FALSE). Delete the cache directory (if
#'   it exists) for each R Markdown file prior to building it.
#' @param seed numeric (default: 12345). The seed to set before building each
#'   file. Passed to \code{\link{set.seed}}. \bold{DEPRECATED:} The seed set
#'   here has no effect if you are using \code{\link{wflow_html}} as the output
#'   format defined in \code{_site.yml}. This argument is for backwards
#'   compatibility with previous versions of workflowr.
#' @param log_dir character (default: NULL). The directory to save log files
#'   from building files. It will be created if necessary and ignored if
#'   \code{local = TRUE}. The default is to use a directory named
#'   \code{workflowr} in \code{\link{tempdir}}.
#' @param verbose logical (default: FALSE). Display the build log directly in
#'   the R console as each file is built. This is useful for monitoring
#'   long-running code chunks.
#' @param local logical (default: FALSE). Build files locally in the R console.
#'   This should only be used for debugging purposes. The default is to build
#'   each file in its own separate fresh R process to ensure each file is
#'   reproducible in isolation.
#' @param dry_run logical (default: FALSE). List the files to be built, without
#'   building them.
#'
#' @inheritParams wflow_git_commit
#'
#' @return An object of class \code{wflow_build}, which is a list with the
#'   following elements:
#'
#'  \itemize{
#'
#'    \item \bold{files}: The input argument \code{files}
#'
#'    \item \bold{make}: The input argument \code{make}
#'
#'    \item \bold{update}: The input argument \code{update}
#'
#'    \item \bold{republish}: The input argument \code{republish}
#'
#'    \item \bold{view}: The input argument \code{view}
#'
#'    \item \bold{clean_fig_files}: The input argument \code{clean_fig_files}
#'
#'    \item \bold{delete_cache}: The input argument \code{delete_cache}
#'
#'    \item \bold{seed}: The input argument \code{seed}
#'
#'    \item \bold{log_dir}: The directory where the log files were saved
#'
#'    \item \bold{verbose}: The input argument \code{verbose}
#'
#'    \item \bold{local}: The input argument \code{local}
#'
#'    \item \bold{dry_run}: The input argument \code{dry_run}
#'
#'    \item \bold{built}: The relative paths to the built R Markdown files
#'
#'    \item \bold{html}: The relative paths to the corresponding HTML files
#'
#'  }
#'
#' @seealso \code{\link{wflow_publish}}
#'
#' @examples
#' \dontrun{
#'
#' # Build any files which have been modified
#' wflow_build() # equivalent to wflow_build(make = TRUE)
#' # Build a single file
#' wflow_build("file.Rmd")
#' # Build multiple files
#' wflow_build(c("file1.Rmd", "file2.Rmd"))
#' # Build multiple files using a file glob
#' wflow_build("file*.Rmd")
#' # Build every published file
#' wflow_build(republish = TRUE)
#' # Build file.Rmd and any files which have been modified
#' wflow_build("file.Rmd", make = TRUE)
#' }
#'
#' @import rmarkdown
#' @export
wflow_build <- function(files = NULL, make = is.null(files),
                        update = FALSE, republish = FALSE,
                        view = getOption("workflowr.view"),
                        clean_fig_files = FALSE, delete_cache = FALSE,
                        seed = 12345, log_dir = NULL, verbose = FALSE,
                        local = FALSE, dry_run = FALSE, project = ".") {

  # Check input arguments ------------------------------------------------------

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

  if (!(is.logical(make) && length(make) == 1))
    stop("make must be a one-element logical vector")

  if (!(is.logical(update) && length(update) == 1))
    stop("update must be a one-element logical vector")

  if (!(is.logical(republish) && length(republish) == 1))
    stop("republish must be a one-element logical vector")

  if (!(is.logical(view) && length(view) == 1))
    stop("view must be a one-element logical vector")

  if (!(is.logical(clean_fig_files) && length(clean_fig_files) == 1))
    stop("clean_fig_files must be a one-element logical vector")

  if (!(is.logical(delete_cache) && length(delete_cache) == 1))
    stop("delete_cache must be a one-element logical vector")

  if (!(is.numeric(seed) && length(seed) == 1))
    stop("seed must be a one element numeric vector")

  if (is.null(log_dir)) {
    log_dir <- file.path(tempdir(), "workflowr")
  } else if (!(is.character(log_dir) && length(log_dir) == 1)) {
    stop("log_dir must be NULL or a one element character vector")
  }
  log_dir <- absolute(log_dir)
  fs::dir_create(log_dir)

  if (!(is.logical(verbose) && length(verbose) == 1))
    stop("verbose must be a one-element logical vector")

  if (!(is.logical(local) && length(local) == 1))
    stop("local must be a one-element logical vector")

  if (!(is.logical(dry_run) && length(dry_run) == 1))
    stop("dry_run must be a one-element logical vector")

  if (!(is.character(project) && length(project) == 1))
    stop("project must be a one-element character vector")

  check_wd_exists()

  if (!fs::dir_exists(project)) {
    stop("project directory does not exist.")
  }

  project <- absolute(project)

  # Check to see if pandoc is installed
  if(!rmarkdown::pandoc_available())
    stop("Pandoc is not installed. Please use RStudio or install pandoc manually")

  if(rmarkdown::pandoc_version() >= 2 && utils::packageVersion("rmarkdown") < 1.7)
    stop("pandoc 2+ requires rmarkdown version >= 1.7. Please update your rmarkdown package")

  if (isTRUE(getOption("workflowr.autosave"))) autosave()

  # Obtain files to consider ---------------------------------------------------

  p <- wflow_paths(project = project)
  # All files to consider (ignore files that start with an underscore)
  files_all <- list.files(path = p$analysis, pattern = "^[^_].*\\.[Rr]md$",
                          full.names = TRUE)
  files_all <- relative(files_all)

  # All files must be in the analysis subdirectory. Since it's possible to
  # directly pass a file that starts with an underscore, the file may not be in
  # files_all.
  if (!is.null(files)) {
    if (!all(dirname(files) == p$analysis)) {
      stop(wrap(
      "Only files in the analysis directory can be built with wflow_build. Use
      `rmarkdown::render` to build non-workflowr files."))
    }
  }

  # Check for site generator ---------------------------------------------------

  index <- file.path(p$analysis, "index.Rmd")
  if (!check_site_generator(index)) {
    m <-
    "
    Missing workflowr-specific site generator. To ensure your workflowr
    project functions properly, please add the following to the YAML header of
    index.Rmd:

    site: workflowr::wflow_site

    "
    warning(wrap(m), call. = FALSE, immediate. = TRUE)
  }

  # Determine which files to build ---------------------------------------------

  files_to_build <- files
  if (make) {
    files_make <- return_modified_rmd(files_all, p$docs)
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
      files_republish <- rownames(s$status)[s$status$published &
                                           !s$status$mod_unstaged &
                                           !s$status$mod_staged]
      files_to_build <- union(files_to_build, files_republish)
    }
  }

  # Build files ----------------------------------------------------------------

  if (!dry_run) {
    n_files <- length(files_to_build)
    if (n_files > 0) {
      wd <- getwd()
      message(sprintf("Current working directory: %s", wd))
      message(sprintf("Building %i file(s):", n_files))
    }
    for (f in files_to_build) {
      # Determine knit directory
      wflow_opts <- wflow_options(f)
      if (wflow_opts$knit_root_dir != wd) {
        message(sprintf("Building %s in %s", f, wflow_opts$knit_root_dir))
      } else {
        message("Building ", f)
      }
      # Remove figure files to prevent accumulating outdated files
      if (clean_fig_files) {
        path <- create_figure_path(f)
        figs_analysis <- file.path(p$analysis, path)
        unlink(figs_analysis, recursive = TRUE)
        figs_docs <- file.path(p$docs, path)
        unlink(figs_docs, recursive = TRUE)
      }
      # Delete the cache directory
      dir_cache <- fs::path_ext_remove(f)
      dir_cache <- glue::glue("{dir_cache}_cache")
      if (fs::dir_exists(dir_cache)) {
        if (delete_cache) {
          wflow_delete(dir_cache)
          message("  - Note: Deleted the cache directory before building")
        } else {
          message("  - Note: This file has a cache directory")
        }
      }
      if (local) {
        build_rmd(f, seed = seed, envir = .GlobalEnv)
      } else {
        build_rmd_external(f, seed = seed, log_dir = log_dir, verbose = verbose,
                           envir = .GlobalEnv)
      }
    }
    # Create .nojekyll if it doesn't exist
    nojekyll <- file.path(p$docs, ".nojekyll")
    if (!fs::file_exists(nojekyll)) {
      fs::file_create(nojekyll)
    }
  }

  # View files -----------------------------------------------------------------

  # When 0 files are built, wflow_build() will do nothing.
  #
  # When 1 file is built, wflow_build() will open that file.
  #
  # When 1+ files are built, wflow_build() will open index.html.
  if (!dry_run && view) {
    n_files_to_build <- length(files_to_build)
    if (n_files_to_build == 1) {
      wflow_view(files_to_build, project = project)
    } else if (n_files_to_build > 1) {
      index <- file.path(p$analysis, "index.Rmd")
      wflow_view(index, project = project)
    }
  }

  # Prepare output -------------------------------------------------------------

  o <- list(files = files, make = make,
            update = update, republish = republish, view = view,
            clean_fig_files = clean_fig_files, delete_cache = delete_cache,
            seed = seed, log_dir = log_dir, verbose = verbose,
            local = local, dry_run = dry_run,
            built = files_to_build,
            html = to_html(files_to_build, outdir = p$docs))
  class(o) <- "wflow_build"

  return(o)
}

#' @export
print.wflow_build <- function(x, ...) {
  cat("Summary from wflow_build\n\n")
  cat("Settings:\n")
  if (x$make) cat(" make: TRUE")
  if (x$update) cat(" update: TRUE")
  if (x$republish) cat(" republish: TRUE")
  if (x$clean_fig_files) cat(" clean_fig_files: TRUE")
  if (x$delete_cache) cat(" delete_cache: TRUE")
  if (x$verbose) cat(" verbose: TRUE")
  if (x$local) cat(" local: TRUE")
  if (x$dry_run) cat(" dry_run: TRUE")
  cat("\n\n")

  if (length(x$built) == 0) {
    cat(wrap("No files to build"))
    cat("\n")
    return(invisible(x))
  }

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
  cat(x$html, sep = "\n")
  if (!x$dry_run & !x$local) {
    cat("\n")
    cat(wrap(sprintf("Log files saved in %s", x$log_dir)))
    cat("\n")
  }

  return(invisible(x))
}

# Return the R Markdown files which have been modified more recently than their
# corresponding HTML files.
#
# Input: character. path to R Markdown files in analysis directory
return_modified_rmd <- function(rmd_files, docs) {

  # Expected html files
  html_files <- to_html(rmd_files, outdir = docs)

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

build_rmd_external <- function(rmd, seed, log_dir, verbose = FALSE, ...) {
  if (!(is.character(rmd) && length(rmd) == 1))
    stop("rmd must be a one element character vector")
  if (!fs::file_exists(rmd))
    stop("rmd does not exist: ", rmd)
  if (!(is.numeric(seed) && length(seed) == 1))
    stop("seed must be a one element numeric vector")
  if (!(is.character(log_dir) && length(log_dir) == 1))
    stop("log_dir must be a one element character vector")
  if (!fs::dir_exists(log_dir)) {
    fs::dir_create(log_dir)
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
                  args = list(rmd, seed, ...),
                  stdout = stdout_file,
                  stderr = stderr_file,
                  show = verbose),
    error = function(e) {
      message(wrap("Build failed. See log files for full details."))
      message("stdout: ", stdout_file)
      message("stderr: ", stderr_file)
      stdout_lines <- readLines(stdout_file)
      n <- length(stdout_lines)
      # Print the final 10 lines of standard out to give context to error
      cat(stdout_lines[pmax(1, n - 10):n], sep = "\n")
      stop(e$message, call. = FALSE)
    }
  )
  return(invisible(rmd))
}

build_rmd <- function(rmd, seed, ...) {
  if (!(is.character(rmd) && length(rmd) == 1))
    stop("rmd must be a one element character vector")
  if (!(is.numeric(seed) && length(seed) == 1))
    stop("seed must be a one element numeric vector")
  if (!fs::file_exists(rmd))
    stop("rmd must exist")

  set.seed(seed)
  rmarkdown::render_site(rmd, ...)
}
