#' Start a new workflowr project.
#'
#' \code{wflow_start} creates a new project directory for the site.
#'
#' This is the initial function that organizes the infrastructure to create a
#' research website for your project. Note that while you do not need to use
#' RStudio with workflowr, do not delete the Rproj file because it is required
#' by other functions.
#'
#' @param name Project name, e.g. "My Project"
#' @param directory The directory for the project, e.g. "new-project". Will be
#'   created if necessary.
#' @param git_init Control whether the directory is initiatlized as a Git
#'   repository (default: TRUE).
#' @param overwrite overwrite	Control whether to overwrite existing files
#'   (default: FALSE). Passed to \code{file.copy}.
#'
#' @examples
#' \dontrun{
#' wflow_start("My Project", "path/to/new-project")
#' }
#' @export
wflow_start <- function(name, directory, git_init = TRUE, overwrite = FALSE) {

  # Create directory if it doesn't already exist
  if (!dir.exists(directory)) {
    dir.create(directory)
  }

  # Copy infrastructure files to new directory
  infrastructure_path <- system.file("infrastructure/",
                                     package = "workflowr")
  # Add . to end of path to copy its contents w/o creating a top-level directory
  # source; http://superuser.com/a/367303/449452
  file.copy(from = paste0(infrastructure_path, "/."), to = directory,
            overwrite = overwrite, recursive = TRUE)

  # Add project name to YAML file
  yml_template <- readLines(file.path(directory, "analysis/_site.yml"))
  writeLines(whisker::whisker.render(yml_template, list(name = name)),
             file.path(directory, "analysis/_site.yml"))

  # Add project name to README.md file
  readme_template <- readLines(file.path(directory, "README.md"))
  writeLines(whisker::whisker.render(readme_template, list(name = name)),
             file.path(directory, "README.md"))

  # Configure RStudio
  rs_version <- check_rstudio_version()
  # If the user is running RStdudio and it is greater than version 1.0, specify
  # that the BuildType is Website.
  if (!is.null(rs_version)) {
    if (rs_version >= "1.0.0") {
      cat("\nBuildType: Website\nWebsitePath: analysis\n",
          file = file.path(directory, "temp-name.Rproj"),
          append = TRUE)
    }
  }
  # Rename RStudio Project file
  file.rename(file.path(directory, "temp-name.Rproj"),
              file.path(directory, paste0(basename(directory), ".Rproj")))

  message("Project \"", name, "\" started in ", directory, "\n")

  # Configure Git repository
  if (git_init) {
    create_gitignore(directory)
    git2r::init(directory)
    message("Git repository initialized.")
    repo <- git2r::repository(directory)
    config_info <- git2r::config()
    if (is.null(config_info$global$user.name) |
        is.null(config_info$global$user.email)) {
      message("\nThe Git user.name and/or user.email have not been configured.")
      message("\nRun ?git2r::config to learn how to update this.")
    } else {
      # Make the first initial commit
      git2r::add(repo, ".")
      git2r::commit(repo, message = "Start workflowr project.")
    }
  }

  return(invisible())
}

check_rstudio_version <- function() {
  if (rstudioapi::isAvailable()) {
    rs_version <- rstudioapi::getVersion()
    if (rs_version < "1.0.0") {
      message(strwrap(sprintf("You can gain lots of new useful features
                              by updating to RStudio version 1.0 or greater.
                              You are running RStudio %s", rs_version)))
    }
  } else {
    rs_version <- NULL
  }
  return(rs_version)
}
