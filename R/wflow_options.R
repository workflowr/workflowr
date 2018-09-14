# Find the workflowr options set in _workflowr.yml (if it exists).
#
# Input: path to R Markdown file
#
# Output: list with following components
#  knit_root_dir - directory to execute code
#  seed - random seed to set at beginning of each analysis
#  github - URL to associated GitHub repository
#  sessioninfo - Function to record session information
#  fig_path_ext - figures directory with or without .Rmd
wflow_options <- function(file) {

  # Default wflow options
  wflow_opts <- list(knit_root_dir = NULL,
                     seed = 12345,
                     github = get_host_from_remote(dirname(file)),
                     sessioninfo = "sessionInfo()",
                     fig_path_ext = FALSE)

  # Get options from a potential _workflowr.yml file
  wflow_root <- try(rprojroot::find_root(rprojroot::has_file("_workflowr.yml"),
                                         path = dirname(file)), silent = TRUE)
  if (class(wflow_root) != "try-error") {
    wflow_yml <- file.path(wflow_root, "_workflowr.yml")
    wflow_yml_opts <- yaml::yaml.load_file(wflow_yml)
    for (opt in names(wflow_yml_opts)) {
      wflow_opts[[opt]] <- wflow_yml_opts[[opt]]
    }
    # If knit_root_dir is a relative path, interpret it as relative to the
    # location of _workflowr.yml
    if (!is.null(wflow_opts$knit_root_dir)) {
      if (!R.utils::isAbsolutePath(wflow_opts$knit_root_dir)) {
        wflow_opts$knit_root_dir <- absolute(file.path(wflow_root,
                                                       wflow_opts$knit_root_dir))
      }
    }
  }

  return(wflow_opts)
}

#' check the fig_path_ext option
#'
#' @param input the path to a RMarkdown file
#'
#' @keywords internal
is_fig_path_ext <- function(input) {

  wflow_opts <- wflow_options(input)

  wflow_opts$fig_path_ext
}

#' create the path to the figure folder
#'
#' @param input the path to a RMarkdown file
#'
#' @keywords internal
create_figure_path <- function(input) {

  if (is_fig_path_ext(input)){
    res <- file.path("figure", basename(tools::file_path_sans_ext(input)))
  } else {
    res <- file.path("figure", basename(input))
  }
  res
}
