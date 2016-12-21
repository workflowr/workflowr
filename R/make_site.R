#' Render only the updated R Markdown files
#'
#' \code{make_site} provides Makefile-like functionality to render only the R
#' Markdown files that have been modified. This is in contrast to
#' \code{rmarkdown::render_site}, which always renders all the pages. You should
#' use the latter if you've made aesthetic changes that you want applied to all
#' the R Markdown files.
#'
#' Under the hood, this runs \code{rmarkdown::render_site} on each updated file
#' individually. This provides all the website styling (which you would lose if
#' you ran \code{rmarkdown::render}) without re-building the entire site (which
#' would happen if you ran \code{rmarkdown::render_site with no arguments}).
#'
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
#' make_site()
#' }
#' @export
make_site <- function(dry_run = FALSE, path = ".", ...) {
  analysis_dir <- rprojroot::find_rstudio_root_file("analysis", path = path)
  stopifnot(dir.exists(analysis_dir))

  # Gather Rmd files
  rmd_files <- Sys.glob(file.path(analysis_dir, "*Rmd"))
  # Expected html files
  html_files <- stringr::str_replace(rmd_files, "Rmd", "html")
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

  if (length(files_to_update) == 0) {
    message("All HTML files have been rendered")
  }

  # Render the updated R Markdown files
  if (dry_run) {
    return(files_to_update)
  } else {
    for (f in files_to_update) {
      cat(sprintf("\n\nRendering %s\n\n", f))
      rmarkdown::render_site(f, ...)
    }
  }

  return(invisible(files_to_update))
}
