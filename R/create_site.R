#' Creates a new site.
#'
#' \code{create_site} creates a new project directory for the site.
#'
#' This is the initial function that organizes the infrastructure to create a
#' research website for your project.
#'
#' @param name Project name, e.g. "My Project"
#' @param directory The directory for the project, e.g. "new-project". Will be
#'   created if necessary.
#' @param git_init Control whether the directory is initiatlized as a Git
#'   repository (default: TRUE).
#' @param rstudio Control whether an RStudio Project file is created (default:
#'   TRUE).
#'
#' @examples
#' \dontrun{
#' create_site("My Project", "path/to/new-project")
#' }
#' @export
create_site <- function(name, directory, git_init = TRUE, rstudio = TRUE) {
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
            recursive = TRUE)

  # Add project name to YAML file
  yml_template <- readLines(file.path(directory, "analysis/_site.yml"))
  writeLines(whisker::whisker.render(yml_template, list(name = name)),
             file.path(directory, "analysis/_site.yml"))

  # Configure RStudio Project
  if (rstudio) {
    # Add RStudio version
    rs_version <- rstudioapi::getVersion()
    rs_version_short <- paste(strsplit(as.character(rs_version), "\\.")[[1]][1:2],
                              collapse = ".")
    rproj_template <- readLines(file.path(directory, "temp-name.Rproj"))
    writeLines(whisker::whisker.render(rproj_template,
                                       list(version = rs_version_short)),
               file.path(directory, "temp-name.Rproj"))
    # Rename RStudio Project file
    file.rename(file.path(directory, "temp-name.Rproj"),
                file.path(directory, paste0(basename(directory), ".Rproj")))
  } else {
    unlink(file.path(directory, "temp-name.Rproj"))
  }

  message("Project ", name, " started in ", directory)

  # Configure Git repository
  if (git_init) {
    git2r::init(directory)
    repo <- git2r::repository(directory)
    # Make the first initial commit?
    # git2r::commit(repo, message = "Initial commit")
    message("Git repository initialized")
  } else {
    unlink(file.path(directory, ".gitignore"))
  }

  return(invisible())
}
