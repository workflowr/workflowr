#' Create table of contents
#'
#' \code{wfow_toc} creates a table of contents of the published R Markdown
#' files. The output is in markdown format, so you can paste it into a document
#' such as \code{index.Rmd}. If the R package
#' \href{https://cran.r-project.org/package=clipr}{clipr} is installed, the
#' table of contents is copied to the clipboard. Otherwise the output is sent to
#' the R console.
#'
#' The default behavior is to attempt to copy the table of contents to the
#' clipboard for easy pasting into an R Markdown document. If this isn't working
#' for you, you can try the following:
#'
#' \itemize{
#'
#' \item Check that the clipr package is installed:
#' \code{install.packages("clipr")}
#'
#' \item Check that the system keyboard is writable. Run
#' \code{\link[clipr]{clipr_available}} and \code{\link[clipr:clipr_available]{dr_clipr}}.
#'
#' }
#'
#' @param ignore_nav_bar logical (default: TRUE). Ignore any HTML files included
#'   as links in the navigation bar.
#' @param clipboard logical (default: TRUE) Attempt to copy table of contents to
#'   clipboard. Only relevant if
#'   \href{https://cran.r-project.org/package=clipr}{clipr} package is installed
#'   and the system keyboard is available.
#' @inheritParams wflow_git_commit
#'
#' @return Invisibly returns the table of contents as a character vector.
#'
#' @export
wflow_toc <- function(ignore_nav_bar = TRUE, clipboard = TRUE, project = ".") {

  # Check input arguments ------------------------------------------------------

  assert_is_flag(ignore_nav_bar)
  assert_is_flag(clipboard)
  check_wd_exists()
  assert_is_single_directory(project)
  project <- absolute(project)

  # Create table of contents ---------------------------------------------------

  s <- wflow_status(project = project)
  rmd <- rownames(s$status)[s$status$published]
  html <- to_html(basename(rmd))

  # Obtains the toc except the documents in the navigation bar.
  if (ignore_nav_bar) {
    yml <- yaml::read_yaml(file.path(s$analysis, "_site.yml"))
    navbar <- unlist(c(yml$navbar$left, yml$navbar$right))
    html_in_nav <- html %in% navbar

    html <- html[!html_in_nav]
    rmd <- rmd[!html_in_nav]
  }

  titles <- vapply(rmd, get_rmd_title, character(1))
  titles <- ifelse(is.na(titles), basename(rmd), titles)
  toc <- glue::glue("1. [{titles}]({html})")
  toc <- as.character(toc)

  # Output ---------------------------------------------------------------------

  write_to_clip <- clipboard &&
                   requireNamespace("clipr", quietly = TRUE) &&
                   interactive() &&
                   clipr::clipr_available()

  if (write_to_clip) {
    clipr::write_clip(toc)
    message("The table of content of your project is on the clipboard.")
  } else {
    message(paste0(toc, collapse = "\n"))
  }

  return(invisible(toc))
}

# Obtains the title in the YAML header of an R Markdown file. If not set,
# returns NA_character_.
get_rmd_title <- function(x) {
  stopifnot(fs::file_exists(x))
  header <- rmarkdown::yaml_front_matter(x)
  if (is.null(header$title)) {
    return(NA_character_)
  } else {
    return(header$title)
  }
}

wflow_toc_addin <- function() {
  if (is.null(rstudioapi::getSourceEditorContext()))
    stop("wflow_toc() addin: No file open. Please open a file to paste the table of contents.",
         call. = FALSE)

  toc <- suppressMessages(wflow_toc(clipboard = FALSE))

  if (length(toc) == 0)
    stop(wrap(
      "wflow_toc() addin: Couldn't find any published files", "(that aren't part
      of the navigation bar). Use wflow_publish() first."),
         call. = FALSE)

  toc_single <- paste(toc, collapse = "\n")
  toc_single <- paste0(toc_single, "\n")
  rstudioapi::insertText(toc_single)
}
