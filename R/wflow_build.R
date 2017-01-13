#' Build the website
#'
#' \code{wflow_build} builds the website by rendering the R Markdown files in
#' the analysis directory and saving them to \code{docs/}. By default it only
#' renders the R Markdown files that have been modified more recently than their
#' corresponding HTML file (similar to a Makefile). To render specific R
#' Markdown files, pass them as a vector to the argument \code{files}. To render
#' every single page (e.g. to change the theme across the entire site), set
#' \code{files = "all"}.
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
#' @param files R Markdown files to be rendered. The files can be specified
#'   using the path or just the basename. Set to "all" to render every R
#'   Markdown file (default: "").
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
#' # Render specific files
#' wflow_build(files = c("one.Rmd", "two.Rmd"))
#' # Render all files
#' wflow_build(files = "all")
#' }
#' @export
wflow_build <- function(files = "", dry_run = FALSE, path = ".", ...) {
  stopifnot(is.character(files),
            is.logical(dry_run),
            is.character(path))
  analysis_dir <- rprojroot::find_rstudio_root_file("analysis", path = path)
  stopifnot(dir.exists(analysis_dir))

  if (files == "" | files == "all") {
    # Gather Rmd files (any file starting with _ is ignored)
    rmd_files <- list.files(path = analysis_dir, pattern = "^[^_].*Rmd$",
                            full.names = TRUE)
  } else {
    rmd_files <- file.path(analysis_dir, basename(files))
    stopifnot(file.exists(rmd_files), grep("Rmd$", rmd_files))
  }

  if (files != "" | files == "all") {
    files_to_update <- rmd_files
  } else {
    files_to_update <- return_modified_rmd(rmd_files)
  }

  if (length(files_to_update) == 0) {
    message("All HTML files have been rendered")
  }

  # Render the updated R Markdown files
  if (dry_run) {
    message("The following R Markdown files would be rendered:")
    return(files_to_update)
  } else {
    for (f in files_to_update) {
      cat(sprintf("\n\nRendering %s\n\n", f))
      rmarkdown::render_site(f, ...)
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
