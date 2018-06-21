# Find the workflowr options set in _workflowr.yml (if it exists).
#
# Input: path to R Markdown file
#
# Output: list with following components
#  knit_root_dir - directory to execute code
#  seed - random seed to set at beginning of each analysis
#  github - URL to associated GitHub repository
#  sessioninfo - Function to record session information
wflow_options <- function(file) {

  # Default wflow options
  wflow_opts <- list(knit_root_dir = NULL,
                     seed = 12345,
                     github = get_github_from_remote(dirname(file)),
                     sessioninfo = "sessionInfo()")

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
