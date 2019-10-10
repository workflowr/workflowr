#' Quickly start a workflowr project
#'
#' \code{wflow_quickstart} converts existing R Markdown file(s) into a
#' workflowr project. Its goal is to quickly demonstrate the usefulness of
#' workflowr.
#'
#' \code{wflow_quickstart} performs the following steps:
#'
#' \itemize{
#'
#' \item Starts a new project with \code{\link{wflow_start}}
#'
#' \item Copies the Rmd file(s) to the subdirectory \code{analysis/}
#'
#' \item Adds link(s) to the results to the main index page
#'
#' \item Publishes the Rmd files with \code{\link{wflow_publish}}
#'
#' \item Configures the remote repository with \code{\link{wflow_use_github}} or
#' \code{\link{wflow_use_gitlab}}
#'
#' }
#'
#' Once it has completed, you can push to the remote service with
#' \code{\link{wflow_git_push}}. Alternatively you can run \code{git push} in the
#' terminal.
#'
#' If you are using GitHub and you chose to not allow workflowr to create the repository for you, then
#' you will have to login to your account and create the new repository yourself. If you're using GitLab,
#' you don't have to worry about this because the new repository will be automatically
#' created when you push.
#'
#' @param files character. The R Markdown file(s) to be copied into the
#'   subdirectory \code{analysis/} of the newly created workflowr project. If
#'   the argument \code{directory} is left as \code{NULL}, the workflowr project
#'   will be named after the first Rmd file. This new directory will be located
#'   in the current working directory. Supports file
#'   \href{https://en.wikipedia.org/wiki/Glob_(programming)}{globbing}.
#' @param username character (default: NULL). The GitHub or GitLab account you
#'   want to use to create the remote Git repository. This is likely your
#'   personal username, but it could also be the name of an organization you
#'   belong to.
#' @param directory character (default: NULL). The path to the directory to
#'   create the workflowr project. This directory will also be used to name the
#'   remote Git repository. If left as \code{NULL}, the name is derived from the
#'   first Rmd file that is passed to the argument \code{files}.
#' @param change_wd character(default: TRUE). Change the working directory to
#'   the newly created workflowr project. Passed to \code{\link{wflow_start}}.
#' @param git.user.name character (default: \code{NULL}). The user name
#'   used by Git to sign commits, e.g., "Ada Lovelace". This setting
#'   only applies to the workflowr project being created. To specify the
#'   global setting for the Git user name, use
#'   \code{\link{wflow_git_config}} instead. When \code{user.name =
#'   NULL}, no user name is recorded for the project, and the global
#'   setting will be used. This setting can be modified later
#'   by running \code{git config --local} in the Terminal.
#' @param git.user.email character (default: \code{NULL}). The email
#'   address used by Git to sign commits, e.g.,
#'   "ada.lovelace@ox.ac.uk". This setting only applies to the workflowr
#'   project being created. To specify the global setting for the Git
#'   email address, use \code{\link{wflow_git_config}} instead. When
#'   \code{user.name = NULL}, no email address is recorded for the
#'   project, and the global setting will be used. This setting can be
#'   modified later by running \code{git config --local} in the Terminal.
#' @param host character. Choose the service for hosting the Git repository.
#'   Must be either "github" for GitHub.com or "gitlab" for GitLab.com.
#' @inheritParams wflow_use_github
#'
#' @return Invisibly returns the absolute path to the newly created workflowr
#'   project.
#'
#' @seealso \link{workflowr}, \code{\link{wflow_start}}, \code{\link{wflow_publish}},
#'          \code{\link{wflow_use_github}}, \code{\link{wflow_use_gitlab}},
#'          \code{\link{wflow_git_push}}
#'
#' @examples
#' \dontrun{
#'
#' wflow_quickstart(files = "existing-analysis.Rmd", username = "your-github-username")
#' }
#'
#' @export
wflow_quickstart <- function(files,
                             username,
                             directory = NULL,
                             change_wd = TRUE,
                             git.user.name = NULL,
                             git.user.email = NULL,
                             host = c("github", "gitlab"),
                             create_on_github = NULL
                             ) {

  message("wflow_quickstart:")

  # Check input arguments ------------------------------------------------------

  if (!(is.character(files) && length(files) > 0))
      stop("files must be a character vector of filenames")
  files <- glob(files)
  if (!all(fs::file_exists(files)))
    stop("Not all files exist. Check the paths to the files")
  files <- absolute(files)

  if (!(is.character(username) && length(username) == 1))
    stop("username must be a one-element character vector")

  if (!is.null(directory))
    if (!(is.character(directory) && length(directory) == 1))
      stop("directory must be NULL or a one element character vector: ", directory)

  # Determine directory --------------------------------------------------------

  if (is.null(directory)) {
    directory <- fs::path_ext_remove(fs::path_file(files[1]))
  }
  directory <- absolute(directory)

  if (fs::dir_exists(directory))
    stop("wflow_quickstart() does not support existing directories")

  # Start the project ----------------------------------------------------------

  start <- wflow_start(directory = directory, change_wd = change_wd,
                       user.name = git.user.name, user.email = git.user.email)
  message(glue::glue("* Started the project with wflow_start() in {directory}/"))

  # Copy the Rmd file(s) -------------------------------------------------------

  if (change_wd) {
    files <- relative(files, start = directory)
  }

  new_path <- file.path(directory, "analysis")
  for (f in files) {
    fs::file_copy(f, new_path)
    message(glue::glue("* Copied {fs::path_file(f)} to {new_path}/"))
  }

  # Add links to index.Rmd -----------------------------------------------------

  index <- file.path(directory, "analysis", "index.Rmd")
  rmd_names <- fs::path_ext_remove(fs::path_file(files))
  html <- paste0(rmd_names, ".html")
  links <- glue::glue("* [{rmd_names}]({html})")
  cat(c("\n", links), file = index, sep = "\n", append = TRUE)

  # Publish the Rmd file(s) ----------------------------------------------------

  message("* Building files")
  publish <- suppressMessages(wflow_publish(files = file.path(directory, "analysis", "*Rmd"),
                                            message = "Quickstart commit from wflow_quickstart()",
                                            project = directory))
  message("* Published the analysis files with wflow_publish()")

  # Configure remote repository ------------------------------------------------

  host <- match.arg(host, choices = c("github", "gitlab"))
  if (host == "github") {
    gh_result <- suppressMessages(wflow_use_github(username = username,
                                                   create_on_github = create_on_github,
                                                   project = directory))
    message("* Configured to host project on GitHub.com")
    if (!gh_result$repo_created) {
       message(glue::glue("To do: Create {username}/{gh_result$repository} on GitHub.com"))
    }
  } else if (host == "gitlab") {
    suppressMessages(wflow_use_gitlab(username = username, project = directory))
    message("* Configured to host on project GitLab.com")
  }

  # Return ---------------------------------------------------------------------

  if (change_wd) {
    message(glue::glue("* Current working directory changed to {directory}/"))
  } else {
    message(glue::glue("* Current working directory is still {getwd()}/"))
  }

  message(glue::glue("* To do: Run wflow_git_push() to push your project to {host}"))

  return(invisible(directory))
}
