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
#' @export

wflow_toc <- function(analysis_dir = "analysis", docs_dir = "docs") {
  rmd_regexp <- '\\.Rmd$'
  html_regexp <- '\\.html$'

  df_rmd <- fs::dir_info(here::here(analysis_dir), type = 'file', regexp = rmd_regexp) %>%
    dplyr::transmute(
      path,
      url = basename(path),
      join_key = stringr::str_remove_all(url, rmd_regexp)
    ) %>%
    dplyr::mutate(name = purrr::map(path,~ rmarkdown::yaml_front_matter(.)$title)) %>%
    filter(!map_lgl(name, is_null)) %>%
    tidyr::unnest() %>%
    dplyr::select(join_key, name)

  df_html <- fs::dir_info(here::here(docs_dir), type = 'file', regexp = html_regexp) %>%
    dplyr::transmute(url = basename(path),
                     join_key = stringr::str_remove_all(url, html_regexp)
    )
  df_html %>%
    dplyr::left_join(df_rmd,
              by = 'join_key') %>%
    dplyr::mutate(
      name = ifelse(is.na(name), url, name)
    ) %>%
    dplyr::mutate(
      link = glue::glue("1. [{name}]({url})")
    ) %>%
    .$link %>%
    clipr::write_clip()
  message('The table of content of your project is on the clipboard.')

}
