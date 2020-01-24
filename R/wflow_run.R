#' Run the code
#'
#' \code{wflow_run} executes the code chunks of an R Markdown file in the
#' current R session without affecting any of the website files. This is meant
#' to be used while interactively developing an analysis. It does not change the
#' working directory or isolate the computation from the current R session. This
#' is analogous to the RStudio option "Run all" to run all the code chunks. Use
#' \code{\link{wflow_publish}} when you are ready to add the results to the
#' website.
#'
#' @param file character (default: \code{NULL}). The R Markdown file to execute.
#' Must have file extension Rmd or rmd. If \code{NULL}, the most recently
#' modified Rmd file will be executed.
#' @param verbose logical (default: \code{TRUE}). Should the lines of code (and
#'   their output) be echoed in the R console as they are executed? This
#'   argument is passed directly to the argument \code{echo} of the function
#'   \code{\link{source}}.
#' @inheritParams wflow_git_commit
#'
#' @return Invisibly returns the path to the Rmd file that was executed
#'
#' @seealso \code{\link{wflow_build}} with argument \code{local = TRUE},
#'   \code{\link{source}} with argument \code{echo = TRUE}
#'
#' @examples
#' \dontrun{
#'
#' # Run the most recently modified Rmd file
#' wflow_run()
#' # Run a specific Rmd file
#' wflow_run("analysis/file.Rmd")
#' }
#'
#' @export
wflow_run <- function(file = NULL, verbose = TRUE, project = ".") {

  file <- process_input_files(file, allow_null = TRUE, rmd_only = TRUE,
                              convert_to_relative_paths = TRUE)

  assert_is_flag(verbose)
  check_wd_exists()
  assert_is_single_directory(project)
  project <- absolute(project)

  # If file not specfied, get the most recently modified file
  if (is.null(file)) {
    p <- wflow_paths(project = project)
    files_analysis <- list.files(path = p$analysis, pattern = "^[^_].+[Rr]md$",
                                 full.names = TRUE)
    files_analysis <- relative(files_analysis)
    files_modified <- fs::file_info(files_analysis)$modification_time
    file <- files_analysis[which.max(files_modified)]
  }

  assert_has_length(file, 1)

  # Determine knit directory
  wd <- getwd()
  wflow_opts <- wflow_options(file)
  if (wflow_opts$knit_root_dir != wd) {
    warning(sprintf("Working directory does not match knit_root_dir: %s",
                    wflow_opts$knit_root_dir),
            call. = FALSE)
  }

  # setwd(wflow_opts$knit_root_dir)
  # on.exit(setwd(wd), add = TRUE)
  r_tmp <- fs::file_temp(pattern = "workflowr-purl-", ext = ".R")
  knitr::purl(file, output = r_tmp, quiet = TRUE, documentation = 0L)
  source(r_tmp, echo = verbose)

  return(invisible(file))
}
