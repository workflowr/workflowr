#' Create table of contents
#'
#' \code{wfow_toc} creates a table of contents of the published R Markdown
#' files. The output is in markdown format, so you can paste it into a document
#' such as \code{index.Rmd}. If the R package clipr is installed, the table of
#' contents is copied to the clipboard. Otherwise the output is printed to the R
#' console.
#'
#' @inheritParams wflow_git_commit
#'
#' @return Invisibly returns the table of contents as a character vector.
#'
#' @export
wflow_toc <- function(project = ".") {
  s <- wflow_status(project = project)
  rmd <- rownames(s$status)[s$status$published]
  html <- to_html(basename(rmd))
  titles <- vapply(rmd, get_rmd_title, character(1))
  titles <- ifelse(is.na(titles), basename(rmd), titles)
  toc <- glue::glue("1. [{titles}]({html})")
  toc <- as.character(toc)

  # output
  if (requireNamespace("clipr", quietly = TRUE) && interactive()) {
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
