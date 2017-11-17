
#' @export
wflow_site <- function(input, encoding = getOption("encoding"), ...) {
  wflow_yml <- file.path(input, "..", "_workflowr.yml")
  wflow_config <- yaml::yaml.load_file(wflow_yml)
  rmarkdown:::default_site(input, encoding, ...)
}

#' @export
wflow_document <- function(...) {
  x <- rmarkdown::html_document(...)
  x$knitr$opts_knit$root.dir <- ".."
  x$knitr$opts_chunk$comment <- NA
  x$knitr$opts_chunk$fig.align <- "center"
  x$knitr$opts_chunk$tidy <- FALSE
  x
}
