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
  s <- wflow_status(project = ".")
  df<- s$status
  df$rmd_path <- rownames(df)
  df <- df[df$published]
  df$html_path <- to_html(df$rmd_path,outdir = s$docs) %>% basename()
  file_name <- sapply(df$rmd_path, function(x) rmarkdown::yaml_front_matter(x)$title) %>%
    unlist() %>%
    na.omit() %>%
    as.data.frame() %>%
    `names<-`('name')
  file_name$rmd_path <- rownames(file_name)
  df <- df %>% dplyr::inner_join(file_name, by = 'rmd_path')
  df$name <- ifelse(is.na(df$name), df$html_path, as.character(df$name))
  df$link <- paste0("1. [",df$name,"](",df$html_path,")")
  clipr::write_clip(df$link)
  message('The table of content of your project is on the clipboard.')
}
