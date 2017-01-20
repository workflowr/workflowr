#' Start a new workflowr project.
#'
#' \code{wflow_start} creates a new project directory for the site.
#'
#' This is the initial function that organizes the infrastructure to create a
#' research website for your project. Note that while you do not need to use
#' RStudio with workflowr, do not delete the Rproj file because it is required
#' by other functions.
#'
#' @param name character. Project name, e.g. "My Project"
#' @param directory character. The directory for the project, e.g.
#'   "~/new-project". Will be created if necessary.
#' @param git logical (default: TRUE). Should Git be used for version control?
#'   If \code{directory} is a new Git repository, \code{wflow_start} will
#'   initialize the repository and make an initial commit. If \code{directory}
#'   is already a Git repository, \code{wflow_start} will make an additional
#'   commit. In both cases, only files needed for the workflowr project will be
#'   included in the commit.
#' @param overwrite logical (default: FALSE). Control whether to overwrite
#'   existing files . Passed to \code{file.copy}.
#'
#' @examples
#' \dontrun{
#' wflow_start("My Project", "path/to/new-project")
#' }
#' @export
wflow_start <- function(name, directory, git = TRUE, overwrite = FALSE) {
  stopifnot(is.character(name),
            is.character(directory),
            is.logical(git),
            is.logical(overwrite))

  # Require that user.name and user.email be set locally or globally
  if (git) {
    check_git_config(path = directory)
  }

  # Create directory if it doesn't already exist
  if (!dir.exists(directory)) {
    dir.create(directory)
  }

  # Copy infrastructure files to new directory
  infrastructure_path <- system.file("infrastructure/",
                                     package = "workflowr")
  project_files <- list.files(path = infrastructure_path, all.files = TRUE,
                              recursive = TRUE)
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
  project_files <- stringr::str_replace(project_files, "temp-name",
                                        basename(directory))

  message("Project \"", name, "\" started in ", directory, "\n")

  # Configure Git repository
  if (git) {
    create_gitignore(directory)
    project_files <- c(project_files, file.path(directory, ".gitignore"))
    if (git2r::in_repository(directory)) {
      warning(sprintf("A .git directory already exists in %s", directory))
    } else {
      git2r::init(directory)
      message("Git repository initialized.")
    }
    repo <- git2r::repository(directory)
    # Make the first workflowr commit
    git2r::add(repo, project_files)
    git2r::commit(repo, message = "Start workflowr project.")
  }

  return(invisible(directory))
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

# Check for user.name and user.email in .gitconfig
#
# path character. Path to repository
#
# If unable to find user.name and user.email, stops the program.
check_git_config <- function(path) {
  stopifnot(is.character(path))
  # Only look for local configuration file if the directory exists and it is a
  # Git repo
  if (dir.exists(path)) {
    look_for_local <- git2r::in_repository(path)
  } else {
    look_for_local <- FALSE
  }

  # Determine if user.name and user.email are set
  if (look_for_local) {
    r <- git2r::repository(path)
    git_config <- git2r::config(r)
    config_email_set <- "user.email" %in% names(git_config$global) |
                        "user.email" %in% names(git_config$local)
    config_name_set <- "user.name" %in% names(git_config$global) |
                       "user.name" %in% names(git_config$local)
  } else {
    git_config <- git2r::config()
    config_email_set <- "user.email" %in% names(git_config$global)
    config_name_set <- "user.name" %in% names(git_config$global)
  }

  if (config_email_set & config_name_set) {
    return(invisible())
  } else {
   stop("You must set your user.name and user.email for Git first\n",
        "to be able to run `wflow_start` with `git = TRUE`.\n",
        "Run the following command in R, replacing the arguments\n",
        "with your name and email address, and then re-run `wflow_start`:\n",
        "\n",
        'git2r::config(global = TRUE, user.name = "Your Name", user.email = "youremailaddress")',
        call. = FALSE)
  }
}
