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
#' @export

wflow_toc <- function(gh_url = "https://jdblischak.github.io", proj_url = "workflowr") {
  # root <- file.path(gh_url,proj_url)
  root <- '.'
  title_regexp <- glue::glue("<title>([\\p{Han}A-z\\.\\s-]+)</title>",
                             .open = '<<',
                             .close = '>>')
  fs::dir_info(here::here("docs"), type = 'file', regexp = '\\.html$') %>%
    dplyr::mutate(
      url = basename(path),
      url = file.path(root,url)
    ) %>%
    dplyr::mutate(
      name = purrr::map(path,~ readr::read_lines(.) %>%
                          stringr::str_subset(title_regexp) %>%
                          stringr::str_match_all(title_regexp)
      )
    ) %>%
    tidyr::unnest() %>%
    dplyr::mutate(
      name = purrr::map_chr(name, ~.[,2])
    ) %>%
    dplyr::select(url, name, dplyr::everything()) %>%
    dplyr::mutate(
      link = glue::glue("1. [{name}]({url})")
    ) %>%
    .$link %>%
    clipr::write_clip()
  message('The table of content of your project is on the clipboard.')

}
