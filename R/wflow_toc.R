#' Add a table of content on the clipboard
#'
#' \code{wfow_toc} add a table of content on the clipboard. The users can paste it
#' in the \code{index.Rmd} document. The table of content includes all \code{html} document
#' in the \code{docs} directory the users want to display.
#'
#' @param gh_url character. The Github Pages root directory.
#' @param proj_url character. The project name.
#'
#' @return text character. The list of table of content in the markdown syntax.
#' @import dplyr
#' @import clipr
#' @import rmarkdown
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
  if (requireNamespace("clipr", quietly = TRUE)) {
    clipr::write_clip(toc)
    message('The table of content of your project is on the clipboard.')
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
