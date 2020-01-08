#' Rename a workflowr project
#'
#' If you want to rename an existing workflowr project, use
#' \code{wflow_rename_proj} to update the name throughout all the project files.
#'
#' \code{wflow_rename_proj} performs the following steps and then commits the
#' changes:
#'
#' \itemize{
#'
#' \item Rename RStudio Project file (\code{.Rproj})
#'
#' \item Update URL of remote repository (see \code{\link{wflow_git_remote}})
#'
#' \item Update project name in the navigation bar (defined in \code{_site.yml})
#'
#' \item Update title of README file
#'
#' \item Rename the project directory itself
#'
#' }
#'
#' After renaming the project with \code{wflow_rename_proj}, you should
#' republish the R Markdown files with \code{wflow_publish(republish = TRUE)}.
#' Also, you should go to the settings of your Git repository on the online Git
#' hosting platform to change its name.
#'
#' @param name character. The new name for the workflowr project.
#' @param rproj logical (default: TRUE). Rename the RStudio Project file.
#' @param remote logical (default: TRUE). Rename the remote URL.
#' @param navbar logical (default: TRUE). Rename the navbar title.
#' @param readme logical (default: TRUE). Rename the README title.
#' @param commit logical (default: TRUE). Commit the changes to Git.
#' @param directory logical (default: TRUE). Rename the project directory.
#' @inheritParams wflow_git_commit
#'
#' @return Invisibly returns the path to the project directory
#'
#' @seealso \code{\link{wflow_publish}}
#'
#' @examples
#' \dontrun{
#'
#' wflow_rename_proj("new-project-name")
#' }
#'
#'@export
wflow_rename_proj <- function(name,
                              rproj = TRUE,
                              remote = TRUE,
                              navbar = TRUE,
                              readme = TRUE,
                              commit = TRUE,
                              directory = TRUE,
                              project = ".") {

  # Check input arguments ------------------------------------------------------

  if (!(is.character(name) && length(name) == 1))
    stop("name must be NULL or a one element character vector: ", name)

  if (!(is.logical(rproj) && length(rproj) == 1))
    stop("rproj must be a one-element logical vector")

  if (!(is.logical(remote) && length(remote) == 1))
    stop("remote must be a one-element logical vector")

  if (!(is.logical(navbar) && length(navbar) == 1))
    stop("navbar must be a one-element logical vector")

  if (!(is.logical(readme) && length(readme) == 1))
    stop("readme must be a one-element logical vector")

  if (!(is.logical(commit) && length(commit) == 1))
    stop("commit must be a one-element logical vector")

  if (!(is.logical(directory) && length(directory) == 1))
    stop("directory must be a one-element logical vector")

  if (!(is.character(project) && length(project) == 1))
    stop("project must be a one-element character vector")

  check_wd_exists()

  if (!fs::dir_exists(project)) {
    stop("project directory does not exist.")
  }

  project <- absolute(project)

  message("Summary from wflow_rename_proj():")

  # Status ---------------------------------------------------------------------

  s <- wflow_status(project = project)

  r <- git2r::repository(path = s$git)

  # Rename RStudio Project file ------------------------------------------------

  if (rproj) {
    rproj_old <- fs::dir_ls(path = s$root, regexp = "\\.Rproj$")
    rproj_new <- relative(file.path(s$root, paste0(name, ".Rproj")))
    if (rproj_new == rproj_old) {
      message("* RStudio Project file already named ", rproj_new)
    } else {
      fs::file_move(rproj_old, rproj_new)
      message("* RStudio Project file renamed to ", rproj_new)
    }
  }

  # Update URL of remote repository --------------------------------------------

  if (remote) {
    remote_avail <- wflow_git_remote(verbose = FALSE, project = project)
    if ("origin" %in% names(remote_avail)) {
      url_old <- remote_avail["origin"]
      url_new <- stringr::str_replace(url_old, "/[:alnum:]+\\.git$",
                                      paste0("/", name, ".git"))
      git2r::remote_set_url(repo = r, name = "origin", url = url_new)
      message("* Remote \"origin\" URL renamed to ", url_new)
    }
  }

  # Update project name in the navigation bar ----------------------------------

  if (navbar) {
    site_yml_fname <- file.path(s$analysis, "_site.yml")
    site_yml <- yaml::yaml.load_file(site_yml_fname)
    site_yml$name <- name
    site_yml$navbar$title <- name
    yaml::write_yaml(site_yml, file = site_yml_fname)
    message("* Renamed project in navigation bar")
  }

  # Update title of README file ------------------------------------------------

  if (readme) {
    readme_fname <- file.path(s$root, "README.md")
    readme_title <- paste("#", name)
    readme_lines <- readLines(readme_fname)
    readme_lines[1] <- paste("#", name)
    writeLines(readme_lines, readme_fname)
    message("* README.md title: ", paste("#", name))
  }

  # Commit changes -------------------------------------------------------------

  if (commit) {
    if (rproj) git2r_add(r, c(rproj_new, rproj_old))
    if (navbar) git2r_add(r, site_yml_fname)
    if (readme) git2r_add(r, readme_fname)
    staged <- git2r::status(r)$staged
    if (length(staged) > 0) {
      commit_rename <- git2r::commit(r, paste("Rename project to", name))
      sha <- commit_rename$sha
      message("* Committed changes in ", stringr::str_sub(sha, 1, 7))
    } else {
      message("* No changes to commit")
    }
  }

  if (any(s$status$published))
    message("* To do: Republish analyses with wflow_publish(republish = TRUE)")

  # Rename project directory ---------------------------------------------------

  dir_path <- absolute(s$root)
  dir_path_parts <- fs::path_split(dir_path)[[1]]
  dir_path_parts[[length(dir_path_parts)]] <- name
  dir_path_new <- fs::path_join(dir_path_parts)
  dir_path_new <- as.character(dir_path_new)

  if (directory) {
    if (dir_path != dir_path_new) {
      wd <- absolute(getwd())
      if (wd == dir_path) { # Cannot rename current directory in Windows
        setwd(fs::path_temp())
        on.exit(setwd(dir_path_new))
      }
      fs::file_move(dir_path, dir_path_new)
      message("* Renamed project directory: ", dir_path_new)
    } else {
      message("* Project directory already named: ", dir_path_new)
    }
  }

  # Return ---------------------------------------------------------------------

  message("* To do: Rename repository in settings of your Git hosting service")

  return(invisible(dir_path_new))
}
