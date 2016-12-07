
#' @export
create_site <- function(name, directory, git_init = TRUE) {
  # Create directory if it doesn't already exist
  if (!dir.exists(directory)) {
    dir.create(directory)
  }

  # Copy infrastructure files to new directory
  infrastructure_path <- system.file("infrastructure/",
                                     package = "workflowr")
  # Add . to end of path to copy its contents w/o creating a top-level directory
  # source; http://superuser.com/a/367303/449452
  file.copy(from = paste0(infrastructure_path, "."), to = directory,
            recursive = TRUE)

  # Rename R Project file
  file.rename(file.path(directory, "temp-name.Rproj"),
              file.path(directory, paste0(name, ".Rproj")))

  message("Project ", name, " started in ", directory)

  # Configure Git repository
  if (git_init) {
    git2r::init(directory)
    repo <- git2r::repository(directory)
    # Make the first initial commit?
    # git2r::commit(repo, message = "Initial commit")
    message("Git repository initialized")
  }

  return(invisible())
}
