#' View research website locally.
#'
#' \code{wflow_view} displays the website locally in your browser.
#'
#' \code{wflow_view} by default opens the file \code{index.html}. To view the
#' most recently modified HTML file, set \code{recent = TRUE}. To specify which
#' file(s) to open, specify either the name(s) of the R Markdown or HTML
#' file(s). The path(s) to the file(s) will be discarded, thus only HTML files
#' in docs directory can be viewed with this function.
#'
#' \code{wflow_view} uses \code{\link{browseURL}} to open the HTML files in the
#' browser. If you wish to do something non-traditional like open an HTML file
#' that is not in the docs directory or not part of a workflowr project, you can
#' use that function directly.
#'
#' @param files character (default: NULL). Name(s) of the specific file(s) to
#'   open in the browser. These can be either the name(s) of the R Markdown
#'   file(s) in the analysis directory or the HTML file(s) in the docs
#'   directory. Also, the full path(s) to the file(s) can be input or just the
#'   basename(s) of the file(s).
#' @param recent logical (default: FALSE). If \code{files = NULL}, display the
#'   HTML file with the most recent modification time. If \code{files = NULL}
#'   and \code{recent = FALSE}, then \code{index.html} is opened.
#' @param dry_run logical (default: FALSE). Do not actually open file(s) in
#'   browser. Mainly useful for testing.
#' @param project character (default: ".") By default the function assumes the
#'   current working directory is within the project. If this is not true,
#'   you'll need to provide the path to the project directory.
#'
#' @return Invisibly returns a character vector of the relative paths to the
#'   HTML files to be opened.
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
#' wflow_view(recent = TRUE)
#'
#' # View a file by specifying the R Markdown file
#' # (the following two are equivalent)
#' wflow_view("fname.Rmd")
#' wflow_view("analysis/fname.Rmd")
#'
#' # View a file by specifying the HTML file
#' # (the following two are equivalent)
#' wflow_view("fname.html")
#' wflow_view("docs/fname.html")
#'
#' # View multiple files
#' wflow_view(c("fname1.Rmd", "fname2.Rmd"))
#' }
#' @export
wflow_view <- function(files = NULL, recent = FALSE, dry_run = FALSE,
                       project = ".") {
  if (!(is.null(files) || (is.character(files) && length(files) >= 1)))
    stop("files must be NULL or a character vector.")
  if (!(is.logical(recent) && length(recent) == 1))
    stop("recent must be a one element logical vector. You entered: ", recent)
  if (!(is.logical(dry_run) && length(dry_run) == 1))
    stop("dry_run must be a one element logical vector. You entered: ", dry_run)
  if (!(is.character(project) && length(project) == 1))
    stop("project must be a one element character vector. You entered: ", project)
  if (!dir.exists(project))
    stop("project does not exist. You entered: ", project)

  files <- absolute(files)
  project <- absolute(project)

  p <- wflow_paths(project = project)
  docs_dir <- p$docs

  # 3 options:
  #
  # 1. View docs/index.html
  # 2. View most recently modified HTML file
  # 3. View specified files
  #
  if (is.null(files) & !recent) {
    # 1. View docs/index.html
    html <- file.path(docs_dir, "index.html")
  } else if (is.null(files) & recent) {
    # 2. View most recently modified HTML file
    html_all <- list.files(path = docs_dir, pattern = "html$",
                           full.names = TRUE)
    html_mtime <- file.mtime(html_all)
    html <- html_all[which.max(html_mtime)]
  } else {
    # 3. View specified files
    ext <- tools::file_ext(files)
    ext_wrong <- !(ext %in% c("Rmd", "rmd", "html"))
    if (any(ext_wrong)) {
      warning("The following files had invalid extensions and cannot be viewed:\n",
              paste(files[ext_wrong], collapse = "\n"))
    }
    files <- files[!ext_wrong]
    ext <- ext[!ext_wrong]
    if (length(files) == 0) {
      stop("None of the files had valid extensions.")
    }
    files <- basename(files)
    files <- file.path(docs_dir, files)
    html <- ifelse(ext == "html", files,
                   paste0(tools::file_path_sans_ext(files), ".html"))

  }

  html_missing <- !file.exists(html)
  if (any(html_missing)) {
    warning("The following HTML files are missing:\n",
            paste(html[html_missing], collapse = "\n"))
  }
  html <- html[!html_missing]

  if (length(html) == 0) {
    stop(wrap("No HTML files were able to viewed.
              Try running `wflow_build()` first."))
  }

  if (!dry_run) {
    for (h in html) {
      utils::browseURL(h)
    }
  }

  return(invisible(html))
}
