# Find the workflowr options set in _workflowr.yml (if it exists).
#
# Input: path to R Markdown file
#
# Output: list with following components
#  knit_root_dir - directory to execute code
#  seed - random seed to set at beginning of each analysis
#  github - URL to associated remote repository (e.g. GitHub or GitLab)
#  sessioninfo - Function to record session information
#  fig_path_ext - figures directory with or without .Rmd
wflow_options <- function(file) {

  # Default wflow options
  wflow_opts <- list(knit_root_dir = NULL,
                     seed = 12345,
                     github = get_host_from_remote(dirname(file)),
                     sessioninfo = "sessionInfo()",
                     fig_path_ext = FALSE,
                     suppress_report = FALSE)

  # Get options from a potential _workflowr.yml file
  wflow_root <- try(rprojroot::find_root(rprojroot::has_file("_workflowr.yml"),
                                         path = dirname(file)), silent = TRUE)
  if (!inherits(wflow_root, "try-error")) {
    wflow_yml <- file.path(wflow_root, "_workflowr.yml")
    wflow_yml_opts <- yaml::yaml.load_file(wflow_yml)
    for (opt in names(wflow_yml_opts)) {
      wflow_opts[[opt]] <- wflow_yml_opts[[opt]]
    }
    # If knit_root_dir is a relative path, interpret it as relative to the
    # location of _workflowr.yml
    if (!is.null(wflow_opts$knit_root_dir)) {
      if (fs::is_absolute_path(wflow_opts$knit_root_dir)) {
        m <-
          "The value of knit_root_dir in _workflowr.yml is an absolute path.
          This means that the workflowr project will only execute on your
          current computer. To facilitate reproducibility on other machines,
          change it to a relative path."
        warning(wrap(m), call. = FALSE)
      } else {
        wflow_opts$knit_root_dir <- absolute(file.path(wflow_root,
                                                       wflow_opts$knit_root_dir))
      }
    }
  }

  # If file exists, get potential options from YAML header. These override the
  # options specified in _workflowr.yml.
  if (fs::file_exists(file)) {
    wflow_opts <- wflow_options_from_file(file, wflow_opts)
  }

  # If knit_root_dir hasn't been configured in _workflowr.yml or the YAML header,
  # set it to the location of the original file
  if (is.null(wflow_opts$knit_root_dir)) {
    wflow_opts$knit_root_dir <- dirname(absolute(file))
  }

  return(wflow_opts)
}

# Check for potential workflowr options in YAML header
#
# file - path to existing R Markdown file
# wflow_opts - list of workflowr options
#
# Returns updated list of workflowr options
#
# See wflow_options() and wflow_html() for more details
wflow_options_from_file <- function(file, wflow_opts = list()) {
  header <- rmarkdown::yaml_front_matter(file)
  header_opts <- header$workflowr
  for (opt in names(header_opts)) {
    wflow_opts[[opt]] <- header_opts[[opt]]
  }
  # If knit_root_dir was specified as a relative path in the YAML header,
  # interpret it as relative to the location of the file
  if (!is.null(wflow_opts$knit_root_dir)) {
    if (!fs::is_absolute_path(wflow_opts$knit_root_dir)) {
      wflow_opts$knit_root_dir <- absolute(file.path(dirname(file),
                                                     wflow_opts$knit_root_dir))
    }
  }

  return(wflow_opts)
}

#' check the fig_path_ext option
#'
#' @param input the path to an R Markdown file
#'
#' @keywords internal
is_fig_path_ext <- function(input) {

  wflow_opts <- wflow_options(input)

  wflow_opts$fig_path_ext
}

#' create the path to the figure folder
#'
#' @param input the path to an R Markdown file
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
