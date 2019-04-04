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
#' @import fs
#' @import glue
#' @import dplyr
#' @import purrr
#' @import readr
#' @import tidyr
#' @import clipr
#' @import here
#' @import rmarkdown
#' @import tibble
#' @export

wflow_toc <- function(project = ".") {
  s <- wflow_status(project = project)
  s$status %>%
    tibble::rownames_to_column(var = 'rmd_path') %>%
    dplyr::filter(published) %>%

    dplyr::mutate(html_path = map_chr(rmd_path, to_html, outdir = s$docs),
                  html_path = map_chr(html_path, fs::path_file)) %>%

    dplyr::mutate(name = purrr::map(rmd_path,~ rmarkdown::yaml_front_matter(.)$title)) %>%
    filter(!map_lgl(name, is_null)) %>%
    tidyr::unnest() %>%

    dplyr::mutate(
      name = ifelse(is.na(name), html_path, name)
    ) %>%
    dplyr::mutate(
      link = glue::glue("1. [{name}]({html_path})")
    ) %>%
    .$link %>%
    clipr::write_clip()
  message('The table of content of your project is on the clipboard.')
}
