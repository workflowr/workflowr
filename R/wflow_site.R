
#' @export
wflow_site <- function(input, encoding = getOption("encoding"), ...) {
  # browser()
  wflow_yml <- file.path(input, "..", "_workflowr.yml")
  wflow_config <- yaml::yaml.load_file(wflow_yml)
  rmarkdown:::default_site(input, encoding, ...)
}
