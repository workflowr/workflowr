#' Update a workflowr project
#'
#' Update an existing workflowr project to workflowr 1.0. If you have an
#' existing project built with a beta version of workflowr (pre-1.0.0), you can
#' use \code{wflow_update} to obtain the latest features. However, if you like
#' your current project the way it is, you can continue to use workflowr as you
#' have been by getting the latest bug fixes from
#' \href{https://jdblischak.github.io/workflowrBeta/}{workflowrBeta}.
#'
#' By default, \code{wflow_update} is run in \code{dry_run} mode so that no
#' unwanted changes are made. Here's how to update an existing project to
#' workflowr 1.0:
#'
#' \preformatted{
#' # Preview the files that will be updated
#' wflow_update()
#' # Update the files
#' wflow_update(dry_run = FALSE)
#' # Preview the updates
#' wflow_build()
#' # Publish the updates
#' wflow_publish("_workflowr.yml", "Update to 1.0", all = TRUE)
#' }
#'
#' Currently \code{wflow_update} checks for the following items:
#'
#' \itemize{
#'
#' \item Adds the site generator \code{site: workflowr::wflow_site} to index.Rmd
#'
#' \item Replaces \code{\link[rmarkdown]{html_document}} with
#' \code{workflowr::wflow_html} in _site.yml and the YAML header of the R
#' Markdown files
#'
#' \item Deletes analysis/chunks.R
#'
#' \item Removes the imported chunks in the R Markdown files
#'
#' \item Adds a _workflowr.yml file, but does not change any of the options (so
#' that your site will continue to produce the same results)
#'
#' \item Removes the workflowr line from include/footer.html (this is now
#' inserted automatically by \code{\link{wflow_html}})
#'
#' }
#'
#' @param dry_run logical (default: TRUE). Preview the files to be updated.
#' @param project character (default: ".") By default the function assumes the
#'   current working directory is within the project. If this is not true,
#'   you'll need to provide the path to the project directory.
#'
#' @return A character vector of the updated files.
#'
#' @examples
#' \dontrun{
#'
#' # Preview the files to be updated
#' wflow_update()
#' # Update the files
#' wflow_update(dry_run = FALSE)
#' }
#'
#' @export
wflow_update <- function(dry_run = TRUE, project = ".") {

  # Check input arguments ------------------------------------------------------

  if (!(is.logical(dry_run) && length(dry_run) == 1))
    stop("dry_run must be a one element logical vector. You entered: ", dry_run)
  if (!(is.character(project) && length(project) == 1))
    stop("project must be a one element character vector. You entered: ", project)
  check_wd_exists()
  if (!fs::dir_exists(project))
    stop("project does not exist. You entered: ", project)

  project <- absolute(project)

  if (dry_run) {
    message("Running wflow_update in dry run mode")
  } else {
    message("Running wflow_update")
  }

  # Setup and safety checks ----------------------------------------------------

  p <- wflow_paths(project = project)
  # Keep track of updated files
  files_updated <- character()

  # _site.yml ------------------------------------------------------------------

  site_yml <- file.path(p$analysis, "_site.yml")
  site_yml_in <- readLines(site_yml)
  site_yml_out <- stringr::str_replace(site_yml_in,
                                       "html_document",
                                       "workflowr::wflow_html")

  if (!identical(site_yml_in, site_yml_out)) {
    files_updated <- c(files_updated, site_yml)
    if (!dry_run) writeLines(site_yml_out, site_yml)
  }

  # index.Rmd ------------------------------------------------------------------

  index <- file.path(p$analysis, "index.Rmd")
  index_in <- readLines(index)
  index_out <- stringr::str_replace(index_in,
                                    "html_document",
                                    "workflowr::wflow_html")
  site_line <- "site: workflowr::wflow_site"
  if (index_out[1] == "---" && !any(stringr::str_detect(index_out, site_line))) {
    index_out <- c(index_out[1],
                   site_line,
                   index_out[2:length(index_out)])
  }

  if (!identical(index_in, index_out)) {
    files_updated <- c(files_updated, index)
    if (!dry_run) writeLines(index_out, index)
  }

  # R Markdown files -----------------------------------------------------------

  rmd_files <- list.files(path = p$analysis, pattern = "^[^_].*\\.[Rr]md$",
                          full.names = TRUE)

  for (rmd in rmd_files) {
    lines_in <- readLines(rmd)
    text_in <- paste(lines_in, collapse = "\n")
    lines_out <- lines_in
    # Remove HTML comments
    lines_out <- lines_out[!stringr::str_detect(lines_out, "^<!--")]
    lines_out <- lines_out[!stringr::str_detect(lines_out, "-->$")]
    # Remove Session information header
    lines_out <- lines_out[!stringr::str_detect(lines_out, "## Session information")]

    text_out <- paste(lines_out, collapse = "\n")
    text_out <- stringr::str_replace(text_out, "html_document", "workflowr::wflow_html")
    # Remove empty code chunks
    text_out <- stringr::str_replace_all(text_out, "\n```\\{r.*\\}\n```", "")
    # Remove read-chunk
    text_out <- stringr::str_replace_all(text_out, "\n```\\{r read-chunk.*\\}\n.*\n```\n", "")

    if (!identical(text_in, text_out)) {
      files_updated <- c(files_updated, rmd)
      if (!dry_run) writeLines(text_out, rmd)
    }
  }

  # _workflowr.yml -------------------------------------------------------------

  wflow_yml <- file.path(p$root, "_workflowr.yml")
  wflow_version <- utils::packageVersion("workflowr")
  wflow_yml_lines <- c(
    "# workflowr options",
    glue::glue("# Version {wflow_version}"),
    "",
    "# See ?wflow_html to learn how to customize workflowr."
  )

  if (!fs::file_exists(wflow_yml)) {
    files_updated <- c(files_updated, wflow_yml)
    if (!dry_run) writeLines(wflow_yml_lines, wflow_yml)
  }

  # footer.html ----------------------------------------------------------------

  footer <- file.path(p$analysis, "include", "footer.html")
  if (fs::file_exists(footer)) {
    footer_in <- readLines(footer)
    footer_out <- footer_in
    footer_out <- stringr::str_replace(footer_out,
                                       'This <a href="http://rmarkdown.rstudio.com">R Markdown</a> site was created with <a href="https://github.com/jdblischak/workflowr">workflowr</a>',
                                       "")
    if (!identical(footer_in, footer_out)) {
      files_updated <- c(files_updated, footer)
      if (!dry_run) writeLines(footer_out, footer)
    }
  }

  # chunks.R -------------------------------------------------------------------

  chunks <- file.path(p$analysis, "chunks.R")
  if (fs::file_exists(chunks)) {
    files_updated <- c(files_updated, chunks)
    if (!dry_run) wflow_delete(chunks)
  }

  # Output ---------------------------------------------------------------------

  files_updated <- unique(sort(files_updated))
  return(files_updated)
}
