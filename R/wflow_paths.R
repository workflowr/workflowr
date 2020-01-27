# wflow_paths
#
# Internal function to obtain relevant paths for workflowr project. The paths
# are relative to the current working directory.
#
# error_git: Should the function fail if it can't find a Git repo.
#
# project: path to the workflowr project
#
# Return a list with following elements:
#
# $root: The root directory of the workflowr project
#
# $analysis: The directory that contains \code{_site.yml} and the R Markdown
# files.
#
# $docs: The directory that contains the HTML files and figures.
#
# $git: The .git directory
wflow_paths <- function(error_git = FALSE, project = ".") {

  o <- list()

  # workflowr root
  project <- absolute(project)
  o$root <- try(rprojroot::find_rstudio_root_file(path = project),
                silent = TRUE)
  if (inherits(o$root, "try-error"))
    stop(wrap(
      "Unable to detect a workflowr project. This could be due to one of the
      following reasons:

      1) The function was not executed inside a workflowr project. Run
      `getwd()` to determine the current working directory. Is the working
      directory a workflowr project or one of its subdirectories?

      2) The RStudio .Rproj file was deleted. workflowr requires an RStudio
      .Rproj file to be located at the root of the project. Was it deleted?"),
      call. = FALSE)

  # Analysis directory with _site.yml
  top_level_files <- list.files(path = o$root, full.names = TRUE)
  subdirs <- top_level_files[fs::dir_exists(top_level_files)]
  site_file <- list.files(path = subdirs, pattern = "^_site.yml$",
                          full.names = TRUE)
  if (length(site_file) == 0) {
    stop(wrap("Unable to find the file _site.yml in the analysis directory.
              Is this a workflowr project?"), call. = FALSE)
  } else if (length(site_file) > 1) {
    stop(wrap("Found more than one _site.yml file. Only one subdirectory at the
              top level of the workflowr project can contain _site.yml."),
         call. = FALSE)
  } else {
    o$analysis <- dirname(site_file)
  }

  # rmarkdown website requires index.Rmd file
  index <- file.path(o$analysis, "index.Rmd")
  if (!fs::file_exists(index)) {
    stop(wrap(glue::glue(
      "Invalid workflowr project. R Markdown websites require an index.Rmd
      file. Unable to locate expected file: {index}")),
    call. = FALSE)
  }

  # docs/ directory
  output_dir <- yaml::yaml.load_file(site_file)$output_dir
  if (is.null(output_dir))
    stop(wrap("Unable to locate the website directory. Make sure to set the
              variable output_dir in the file _site.yml"),
         call. = FALSE)
  o$docs <- absolute(file.path(o$analysis, output_dir))

  # Git repository
  r <- try(git2r::repository(o$root, discover = TRUE), silent = TRUE)
  if (inherits(r, "try-error")) {
    if (error_git) {
      stop(wrap("A Git repository is required for this functionality."),
           call. = FALSE)
    } else {
      o$git <- NA_character_
    }
  } else {
    o$git <- absolute(git2r::workdir(r))
  }

  # Make paths relative to working directory
  o <- lapply(o, relative)

  return(o)
}
