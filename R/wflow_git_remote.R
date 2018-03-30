#' Manage remote Git repositories
#'
#' \code{wflow_git_remote} is a convenience function for managing remote
#' repositories from R. By default is displays the current remote repositories
#' (analogous to \code{git remote -v}). It can add a remote, remove a remote, or
#' update the URL for an existing remote.
#'
#' \code{wflow_git_remote} constructs a URL to a remote GitHub repository based on
#' the input GitHub username, GitHub repository name, and protocol (https or
#' ssh). It can add a remote (\code{action = "add"}), remove a remote
#' (\code{action = "remove"}), or update the URL for an existing remote
#' (\code{action = "set_url"}).
#'
#' This function cannot change the name of an existing remote. To accomplish
#' this, you could run Git from the command line (\code{git remote rename <old>
#' <new>}) or use \code{git2r::remote_rename} from R.
#'
#' @param remote character (default: NULL). The name of the remote.
#' @param user character (default: NULL). The GitHub username for the remote
#'   repository.
#' @param repo character (default: NULL). The name of the remote repository on
#'   GitHub.
#' @param protocol character (default: "https"). The protocol for communicating
#'   with GitHub. Must be either "https" or "ssh".
#' @param action character (default: "add"). The action to perform on the
#'   remotes. Must be one of "add", "remove", or "set_url". This argument is
#'   ignored if \code{remote = NULL}.
#' @param verbose logical (default: TRUE). Display the current remotes.
#'   Analogous to \code{git remote -v}.
#' @param project character (default: ".") By default the function assumes the
#'   current working directory is within the project. If this is not true,
#'   you'll need to provide the path to the project directory.
#'
#' @return Invisibly returns a named character vector of the remote URLs.
#'
#' @examples
#' \dontrun{
#' # Display the current remotes
#' wflow_git_remote()
#' # Add a remote called origin that points to the
#' # GitHub repository example_repo owned by
#' # the GitHub user example_user
#' wflow_git_remote("origin", "example_user", "example_repo")
#' # Remove the remote named upstream
#' wflow_git_remote("upstream", action = "remove")
#' # Change the protocol of the remote origin from https to ssh
#' wflow_git_remote("origin", "example_user", "example_repo", protocol = "ssh",
#'               action = "set_url")
#' }
#' @export
wflow_git_remote <- function(remote = NULL, user = NULL, repo = NULL,
                          protocol = "https", action = "add",
                          verbose = TRUE, project = ".") {
  if (!(is.null(remote) | (is.character(remote) & length(remote) == 1)))
    stop("remote must be a one element character vector. You entered: ", remote)
  if (any(stringr::str_detect(remote, c("[:blank:]", "[:punct:]"))))
    stop("Limit the remote name to alphanumeric characters to avoid errors.\n",
         "You entered: ", remote)
  if (!(is.null(user) | (is.character(user) & length(user) == 1)))
    stop("user must be a one element character vector. You entered: ", user)
  if (!(is.null(repo) | (is.character(repo) & length(repo) == 1)))
    stop("repo must be a one element character vector. You entered: ", repo)
  if (!(protocol %in% c("https", "ssh")))
    stop("protocol must be either https or ssh. You entered: ", protocol)
  if (!(action %in% c("add", "remove", "set_url")))
    stop("action must be add, remove, or set_url. You entered: ", action)
  if (!is.logical(verbose) | length(verbose) != 1)
    stop("verbose must be a one element logical vector. You entered: ", verbose)
  if (!is.character(project) | length(project) != 1)
    stop("project must be a one element character vector. You entered: ", project)
  if (!dir.exists(project))
    stop("project does not exist. You entered: ", project)

  project <- absolute(project)

  if (git2r::in_repository(project)) {
    r <- git2r::repository(project, discover = TRUE)
  } else{
    stop("The specified path to the project is not in a Git repository: ",
         project)
  }

  remotes_current <- git2r::remotes(r)

  # Add, remove, or change URL based on value of `action`
  if (action == "add" & !is.null(remote)) {
    if (remote %in% remotes_current)
      stop(remote, " is already defined as a remote.\n",
           "Use `action = \"set_url\"` to update the URL.")
    if (is.null(user) | is.null(repo))
      stop("Must specify both `user` and `repo` to add remote.")
    remote_url <- create_remote_url(user, repo, protocol)
    git2r::remote_add(r, remote, remote_url)
  } else if (action == "remove" & !is.null(remote)) {
    if (remote %in% remotes_current) {
      git2r::remote_remove(r, remote)
    } else {
      stop(remote, " is not defined as a remote. Unable to remove.")
    }
  } else if (action == "set_url" & !is.null(remote)) {
    if (!(remote %in% remotes_current))
      stop(remote, " is not defined as a remote.\n",
           "Use `action = \"add\"` to add it.")
    if (is.null(user) | is.null(repo))
      stop("Must specify both `user` and `repo` to change URL with set_url.")
    remote_url <- create_remote_url(user, repo, protocol)
    git2r::remote_set_url(r, remote, remote_url)
  }

  remotes <- git2r::remotes(r)
  urls <- git2r::remote_url(r, remotes)
  remote_df <- data.frame(name = remotes, url = urls)

  # Output a table of the current remote repositories
  if (verbose) {
    if (nrow(remote_df) > 0) {
      remote_df_string <- utils::capture.output(print(remote_df, quote = FALSE,
                                                      row.names = FALSE))
      remote_df_string <- paste(remote_df_string, "\n", sep = "")
      message("The repository has the following remotes set:\n\n",
              remote_df_string)
    } else {
      message("The repository has no remotes set.")
    }
  }

  # Return invisibly a named character vector of the remote URLs.
  names(urls) <- remotes
  return(invisible(urls))
}

# Create remote URLs.
#
# Examples:
# > workflowr:::create_remote_url("fakename", "fakerepo", "https")
# [1] "https://github.com/fakename/fakerepo.git"
#
# > workflowr:::create_remote_url("fakename", "fakerepo", "ssh")
# [1] "git@github.com:fakename/fakerepo.git"
#
create_remote_url <- function(user, repo, protocol) {
  remote_url <- paste0(user, "/", repo, ".git")
  if (protocol == "https") {
    remote_url <- paste0("https://github.com/", remote_url)
  } else if (protocol == "ssh") {
    remote_url <- paste0("git@github.com:", remote_url)
  } else {
    stop("Invalid input for protocol. You entered: ", protocol)
  }
  return(remote_url)
}
