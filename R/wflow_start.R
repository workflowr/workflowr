#' Start a new workflowr project.
#'
#' \code{wflow_start} creates a minimal workflowr project. The default
#' behaviour is to add these files to a new directory, but it is also
#' possible to populate an already existing project. By default, it
#' also changes the working directory to the workflowr project.
#'
#' This is the initial function that organizes the infrastructure to
#' create a research website for your project. Note that while you do
#' not need to use RStudio with workflowr, do not delete the Rproj
#' file because it is required by other functions.
#'
#' @param directory character. The directory for the project, e.g.
#'   "~/new-project". When \code{existing = FALSE}, the directory will
#'   be created.
#'
#' @param name character (default: NULL). Project name, e.g. "My Project". When
#'   \code{name = NULL}, the project name is automatically set based on the
#'   argument \code{directory}. For example, if \code{directory =
#'   "~/Desktop/myproject"}, then \code{name} is set to \code{"myproject"}.
#'   \code{name} is displayed on the site's navigation bar and the README.md.
#'
#' @param git logical (default: TRUE). Should Git be used for version
#'   control? If \code{directory} is a new Git repository and \code{git
#'   = TRUE}, \code{wflow_start} will initialize the repository and make
#'   an initial commit. If \code{git = TRUE} and \code{directory} is
#'   already a Git repository, \code{wflow_start} will make an
#'   additional commit. In both cases, only files needed for the
#'   workflowr project will be included in the commit.
#'
#' @param existing logical (default: FALSE). Indicate if the specified
#'   \code{directory} already exists. The default prevents injecting the
#'   workflowr files into an unwanted location. Only set to TRUE if you wish to
#'   add the workflowr files to an existing project.
#' @param overwrite logical (default: FALSE). Control whether to overwrite
#'   existing files. Only relevant if \code{existing = TRUE}. Passed to
#'   \code{file.copy}.
#' @param change_wd logical (default: TRUE). Change the working directory to the
#'   \code{directory}.
#'
#' @return Invisibly returns absolute path to workflowr project.
#'
#' @seealso vignette("wflow-01-getting-started")
#'
#' @examples
#' \dontrun{
#'
#' wflow_start("path/to/new-project")
#'
#' # Provide a custom name for the project.
#' wflow_start("path/to/new-project", name = "My Project")
#'
#' # Add workflowr files to an existing project.
#' wflow_start("path/to/current-project", existing = TRUE)
#'
#' # Add workflowr files to an existing project, but do not automatically
#' # commit them.
#' wflow_start("path/to/current-project", git = FALSE, existing = TRUE)
#' }
#' @export
wflow_start <- function(directory,
                        name = NULL,
                        git = TRUE,
                        existing = FALSE,
                        overwrite = FALSE,
                        change_wd = TRUE) {
  if (!is.character(directory) | length(directory) != 1)
    stop("directory must be a one element character vector: ", directory)
  if (!(is.null(name) | (is.character(name) | length(name) != 1)))
    stop("name must be NULL or a one element character vector: ", name)
  if (!is.logical(git) | length(git) != 1)
    stop("git must be a one element logical vector: ", git)
  if (!is.logical(existing) | length(existing) != 1)
    stop("existing must be a one element logical vector: ", existing)
  if (!is.logical(overwrite) | length(overwrite) != 1)
    stop("overwrite must be a one element logical vector: ", overwrite)
  if (!is.logical(change_wd) | length(change_wd) != 1)
    stop("change_wd must be a one element logical vector: ", change_wd)

  if (!existing & dir.exists(directory)) {
    stop("Directory already exists. Set existing = TRUE if you wish to add workflowr files to an already existing project.")
  } else if (existing & !dir.exists(directory)) {
    stop("Directory does not exist. Set existing = FALSE to create a new directory for the workflowr files.")
  }

  # Ensure Windows paths use forward slashes
  directory <- convert_windows_paths(directory)

  # A workflowr directory cannot be created within an existing Git repository if
  # git = TRUE & existing = FALSE.
  if (git & !existing) {
    # In order to check if location is within an existing Git repository, first
    # must obtain the most upstream existing directory
    dir_existing <- obtain_existing_path(directory)
    if (git2r::in_repository(dir_existing)) {
      r <- git2r::repository(dir_existing, discover = TRUE)
      stop("The directory where you have chosen to create a new workflowr directory is already within a Git repository. This is potentially dangerous. If you want to have a workflowr project created within this existing Git repository, re-run wflow_start with `git = FALSE` and then manually commit the new files. The following directory contains the existing .git directory: ", dirname(r@path))
    }
  }

  # Require that user.name and user.email be set locally or globally
  if (git) {
    check_git_config(path = directory)
  }

  # Create directory if it doesn't already exist
  if (!existing & !dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }

  # Convert to absolute path
  directory <- tools::file_path_as_absolute(directory)

  # Configure name of workflowr project
  if (is.null(name)) {
    name <- basename(directory)
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

  # Create a default .Rprofile file
  create_rprofile <- function(path, overwrite = FALSE) {
    lines <-  c(
      "## This makes sure that R loads the workflowr package",
      "## automatically, everytime the project is loaded",
      "if (require(\"workflowr\", quietly = T)) {",
      "  message(\"Loading .Rprofile for the current workflowr project\")",
      "  library(workflowr)", "} else {", "  message(\"workflowr package not installed, please run devtools::install_github('jdblischak/workflowr') to use the workflowr functions\")",
      "}")

    fname <- file.path(path, ".Rprofile")
    exists <- file.exists(fname)
    if (exists & !overwrite) {
      warning(sprintf("File %s already exists. Set overwrite = TRUE to replace",
                      fname))
    } else {
      writeLines(lines, con = fname)
    }
    return(invisible(fname))
  }
  create_rprofile(directory, overwrite = overwrite)

  # Create docs/ directory
  dir.create(file.path(directory, "docs"), showWarnings = FALSE)

  # Create .nojekyll files in analysis/ and docs/ directories
  nojekyll_analysis <- file.path(directory, "analysis", ".nojekyll")
  file.create(nojekyll_analysis)
  nojekyll_docs <- file.path(directory, "docs", ".nojekyll")
  file.create(nojekyll_docs)
  project_files <- c(project_files, nojekyll_analysis, nojekyll_docs)

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

  # Rename RStudio Project file
  file.rename(file.path(directory, "temp-name.Rproj"),
              file.path(directory, paste0(basename(directory), ".Rproj")))
  project_files <- stringr::str_replace(project_files, "temp-name",
                                        basename(directory))

  message("Project \"", name, "\" started in ", directory, "\n")

  # Change working directory to workflowr project
  if (change_wd) {
    setwd(directory)
  } else {
    message("Did not change working directory.\n",
            "Current working directory: ", getwd())
  }

  # Configure Git repository
  if (git) {
    create_gitignore(directory, overwrite = overwrite)
    project_files <- c(project_files, file.path(directory, ".gitignore"))
    if (git2r::in_repository(directory)) {
      warning("A .git directory already exists in ", directory)
    } else {
      git2r::init(directory)
      message("Git repository initialized.")
    }
    repo <- git2r::repository(directory)
    # Make the first workflowr commit
    git2r::add(repo, project_files, force = TRUE)
    status <- git2r::status(repo)
    if (length(status$staged) == 0) {
      warning("No new workflowr files were committed.")
    } else{
      git2r::commit(repo, message = "Start workflowr project.")
    }
  }

  return(invisible(directory))
}

check_rstudio_version <- function() {
  if (rstudioapi::isAvailable()) {
    rs_version <- rstudioapi::getVersion()
    if (rs_version < "1.0.0") {
      message(strwrap(paste("You can gain lots of new useful features",
                        "by updating to RStudio version 1.0 or greater.",
                        "You are running RStudio",
                        as.character(rs_version)), prefix = "\n"))
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
