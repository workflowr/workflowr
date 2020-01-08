#' View research website locally
#'
#' \code{wflow_view} displays the website locally in your browser or the RStudio
#' Viewer pane.
#'
#' \code{wflow_view} by default displays the file \code{index.html}. To view the
#' most recently modified HTML file, set \code{latest = TRUE}. To specify which
#' file(s) to view, specify either the name(s) of the R Markdown or HTML
#' file(s).
#'
#' \code{wflow_view} uses \code{\link{browseURL}} to view the HTML files in the
#' browser. If you wish to do something non-traditional like view an HTML file
#' that is not in the docs directory or not part of a workflowr project, you can
#' use that function directly.
#'
#' If \code{wflow_view} is run in the RStudio IDE and only one file has been
#' requested to be viewed, the file is displayed in the
#' \href{https://rstudio.github.io/rstudio-extensions/rstudio_viewer.html}{RStudio
#' Viewer}.
#'
#' If R has no default browser set (determined by \code{getOption("browser")}),
#' then \code{wflow_view} cannot open any HTML files. See
#' \code{\link{browseURL}} for setup instructions.
#'
#' @param files character (default: NULL). Name(s) of the specific file(s) to
#'   view. These can be either the name(s) of the R Markdown file(s) in the
#'   analysis directory or the HTML file(s) in the docs directory. Supports file
#'   \href{https://en.wikipedia.org/wiki/Glob_(programming)}{globbing}.
#' @param latest logical (default: FALSE). Display the HTML file with the most
#'   recent modification time (in addition to those specified in \code{files}).
#'   If \code{files = NULL} and \code{latest = FALSE}, then \code{index.html} is
#'   viewed.
#' @param dry_run logical (default: FALSE). Do not actually view file(s). Mainly
#'   useful for testing.
#' @param project character (default: ".") By default the function assumes the
#'   current working directory is within the project. If this is not true,
#'   you'll need to provide the path to the project directory.
#'
#' @return An object of class \code{wflow_view}, which is a list with the
#'   following elements:
#'
#'   \item{files}{The input argument \code{files} (converted to relative paths).}
#'
#'   \item{latest}{The input argument \code{latest}.}
#'
#'   \item{dry_run}{The input argument \code{dry_run}.}
#'
#'   \item{browser}{Logical indicating if a default browser has been set. If
#'   FALSE, no HTML files can be opened. This is determined by the value
#'   returned by \code{getOption("browser")}.}
#'
#'   \item{opened}{The HTML files opened by \code{wflow_view}.}
#'
#' @seealso \code{\link{browseURL}}
#'
#' @examples
#' \dontrun{
#'
#' # View index.html
#' wflow_view()
#'
#' # View the most recently modified HTML file
#' wflow_view(latest = TRUE)
#'
#' # View a file by specifying the R Markdown file
#' wflow_view("analysis/fname.Rmd")
#'
#' # View a file by specifying the HTML file
#' wflow_view("docs/fname.html")
#'
#' # View multiple files
#' wflow_view(c("fname1.Rmd", "fname2.Rmd"))
#' wflow_view("docs/*html")
#' }
#' @export
wflow_view <- function(files = NULL, latest = FALSE, dry_run = FALSE,
                       project = ".") {

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
    ext_wrong <- !(ext %in% c("Rmd", "rmd", "html"))
    if (any(ext_wrong))
      stop(wrap("File extensions must be either Rmd, rmd, or html."))
  }

  if (!(is.logical(latest) && length(latest) == 1))
    stop("latest must be a one element logical vector. You entered: ", latest)
  if (!(is.logical(dry_run) && length(dry_run) == 1))
    stop("dry_run must be a one element logical vector. You entered: ", dry_run)
  if (!(is.character(project) && length(project) == 1))
    stop("project must be a one element character vector. You entered: ", project)
  check_wd_exists()
  if (!fs::dir_exists(project))
    stop("project does not exist. You entered: ", project)

  project <- absolute(project)

  p <- wflow_paths(project = project)

  # Require that any R Markdown files are in the R Markdown directory and the
  # HTML files are in the website directory
  if (!is.null(files)) {
    for (i in seq_along(files)) {
      if (ext[i] == "html") {
        if (!stringr::str_detect(files[i], p$docs)) {
          stop("Cannot view non-workflowr file: ", files[i])
        }
      } else {
        if (!stringr::str_detect(files[i], p$analysis)) {
          stop("Cannot view non-workflowr file: ", files[i])
        }
      }
    }
  }

  # Obtain files ---------------------------------------------------------------

  html <- files

  # Convert any R Markdown files to HTML and remove duplicates
  if (!is.null(html)) {
    # `ext` was created during the error handling at the start of the function
    html[ext != "html"] <- to_html(html[ext != "html"], outdir = p$docs)
    html <- unique(html)
  }

  # Obtain the most recently modified file
  if (latest) {
    html_all <- list.files(path = p$docs, pattern = "html$",
                           full.names = TRUE)
    html_mtime <- file.mtime(html_all)
    html <- unique(c(html, html_all[which.max(html_mtime)]))
  }

  # Open the index page if no other files specified
  if (length(html) == 0) {
    html <- file.path(p$docs, "index.html")
  }

  # Check for misssing HTML files ----------------------------------------------

  html_missing <- !fs::file_exists(html)
  if (any(html_missing)) {
    warning("The following HTML files are missing:\n",
            paste(html[html_missing], collapse = "\n"))
  }
  html <- html[!html_missing]

  if (length(html) == 0) {
    stop(wrap("No HTML files were able to viewed.
              Try running `wflow_build()` first."))
  }

  # Check default browser ------------------------------------------------------

  # If no option is set for browser, browseURL will throw an error. This is
  # disastrous if wflow_view was called from wflow_publish because it resets
  # everything it had done if there is an error.
  browser <- check_browser()

  # View files -----------------------------------------------------------------

  if (!dry_run) {
    # If run in RStudio and only 1 file to be viewed, use the RStudio Viewer
    viewer <- getOption("viewer")
    if (!is.null(viewer) && length(html) == 1) {
      # RStudio Viewer only displays files saved in the R temporary directory
      # (and it isn't fooled by symlinks).
      tmp_dir <- absolute(tempdir())
      file.copy(p$docs, tmp_dir, recursive = TRUE)
      html_tmp <- file.path(tmp_dir, basename(p$docs), basename(html))
      viewer(html_tmp)
    } else if (browser) { # Use the default browser
      for (h in html) {
        utils::browseURL(h)
      }
    }
  }

  # Prepare output -------------------------------------------------------------

  o <- list(files = files, latest = latest, dry_run = dry_run,
            browser = browser, opened = html)
  class(o) <- "wflow_view"

  return(o)
}

#' @export
print.wflow_view <- function(x, ...) {

  if (!x$browser) {
    cat(wrap(
      "wflow_view will not open any files because no option is set for a
      default browser. See the Details section of ?browseURL for setup
      instructions."), sep = "\n")

    return(invisible(x))
  }

  if (x$dry_run) {
    cat("wflow_view would open:\n")
  } else {
    cat("wflow_view opened:\n")
  }

  for (f in x$opened) {
    cat(sprintf("- %s\n", f))
  }

  return(invisible(x))
}
