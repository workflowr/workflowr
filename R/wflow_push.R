#' Push files to remote repository
#'
#' \code{wflow_push} pushes the local files on your machine to your remote
#' repository on GitHub. This is a convenience function to run Git commands from
#' the R console instead of the shell. The same functionality can be acheived by
#' running \code{git push} in the Terminal.
#'
#' \code{wflow_push} tries to guess sensible defaults for the remote repository
#' and the branch if the input arguments \code{remote} and \code{branch} are
#' left as \code{NULL}. If there is only one remote or one branch, it is used.
#' If there is more than one remote, the one named "origin" is used. If there is
#' more than one branch, the one named "master" is used.
#'
#' Under the hood, \code{wflow_push} is a wrapper for \code{\link[git2r]{push}}
#' from the package \link{git2r}.
#'
#' @param remote character (default: NULL). The name of the remote repository.
#'   See Details for the default behavior.
#' @param branch character (default: NULL). The name of the branch to push to in
#'   the remote repository. See Details for the default behavior.
#' @param username character (default: NULL). GitHub username. The user is
#'   prompted if necessary.
#' @param password character (default: NULL). GitHub password. The user is
#'   prompted if necessary.
#' @param force logical (default: FALSE). Force the push to the remote
#'   repository. Do not use this if you are not 100\% sure of what it is doing.
#'   Equivalent to: \code{git push -f}
#' @param dry_run logical (default: FALSE). Preview the proposed action but do
#'   not actually push to the remote repository.
#' @param project character (default: ".") By default the function assumes the
#'   current working directory is within the project. If this is not true,
#'   you'll need to provide the path to the project directory.
#'
#' @return An object of class \code{wflow_push}, which is a list with the
#'   following elements:
#'
#'   \itemize{
#'
#'   \item \bold{remote}: The remote repository.
#'
#'   \item \bold{branch}: The branch of the remote repository.
#'
#'   \item \bold{username}: GitHub username.
#'
#'   \item \bold{force}: The input argument \code{force}.
#'
#'   \item \bold{dry_run}: The input argument \code{dry_run}.
#'
#'   }
#'
#' @examples
#' \dontrun{
#'
#' # Push to remote repository
#' wflow_push()
#' # Preview by running in dry run mode
#' wflow_push(dry_run = TRUE)
#' }
#'
#' @export
wflow_push <- function(remote = NULL, branch = NULL,
                       username = NULL, password = NULL,
                       force = FALSE, dry_run = FALSE, project = ".") {

  # To do:
  #   * Use `interactive()` to determine if the user can be prompted

  # Check input arguments ------------------------------------------------------

  if (!(is.null(remote) || (is.character(remote) && length(remote) == 1)))
    stop("remote must be NULL or a one-element character vector")

  if (!(is.null(branch) || (is.character(branch) && length(branch) == 1)))
    stop("branch must be NULL or a one-element character vector")

  if (!(is.null(username) || (is.character(username) && length(username) == 1)))
    stop("username must be NULL or a one-element character vector")

  if (!(is.null(password) || (is.character(password) && length(password) == 1)))
    stop("password must be NULL or a one-element character vector")

  if (!(is.logical(force) && length(force) == 1))
    stop("force must be a one-element logical vector")

  if (!(is.logical(dry_run) && length(dry_run) == 1))
    stop("dry_run must be a one-element logical vector")

  if (is.character(project) && length(project) == 1) {
    # Ensure Windows paths use forward slashes
    project <- convert_windows_paths(project)
    if (dir.exists(project)) {
      project <- normalizePath(project)
    } else {
      stop("project directory does not exist.")
    }
  } else {
    stop("project must be a one-element character vector")
  }

  # Assess status of repository ------------------------------------------------

  # Must be using Git
  p <- wflow_paths(error_git = TRUE, project = project)
  r <- git2r::repository(path = p$git)
  remote_avail <- wflow_remotes(verbose = FALSE, project = project)
  branch_avail <- get_branches(r)

  # Determine remote -----------------------------------------------------------

  # If remote is not specified try to guess the best default using the following
  # criteria:
  #
  # 1. If there are no remotes available, throw an error.
  #
  # 2. If there is only 1 remote available, use it.
  #
  # 3. If there is more than 1 remote available, use the one named "origin"
  #
  # 4. If there is more than 1 remote available, and none is named "origin",
  # throw an error.
  if (is.null(remote)) {
    if (length(remote_avail) == 0) {
      m <- "No remote was specified and none are availabe for this repository.
           Run ?wflow_remotes to read the documentation for setting this up."
      stop(wrap(m))
    } else if (length(remote_avail) == 1) {
      remote <- names(remote_avail)
    } else if (length(remote_avail) > 1) {
      if ("origin" %in% names(remote_avail)) {
        remote <- "origin"
      } else {
        m <- "Multiple remotes are available without an obvious default. Run
             ?wflow_push to read the documentation on pushing to remote
             repositories."
        stop(wrap(m))
      }
    }
  } else {
    if (!(remote %in% c(remote_avail, names(remote_avail)))) {
      m <- sprintf("%s is not one the saved remotes for this Git repository.
                   If the push  fails, this is probably the reason. Run
                   ?wflow_remotes for how to setup a remote.",
                   remote)
      warning(wrap(m))
    }
  }

  # Determine branch -----------------------------------------------------------

  # If branch is not specified try to guess the best default using the following
  # criteria:
  #
  # 1. If there are no branches available, throw an error.
  #
  # 2. If there is only 1 branch available, use it.
  #
  # 3. If there is more than 1 branch available, use the one named "master"
  #
  # 4. If there is more than 1 branch available, and none is named "master",
  # throw an error.
  if (is.null(branch)) {
    if (length(branch_avail) == 0) {
      m <- "No branch was specified and none are availabe for this repository.
           You'll need to fix your Git repository. Start by running `git status`
           from the Terminal."
      stop(wrap(m))
    } else if (length(branch_avail) == 1) {
      branch <- branch_avail
    } else if (length(branch_avail) > 1) {
      if ("master" %in% branch_avail) {
        branch <- "master"
      } else {
        m <- "Multiple branchs are available without an obvious default. Run
             ?wflow_push to read the documentation on pushing to remote
             repositories."
        stop(wrap(m))
      }
    }
  } else {
    if (!(branch %in% branch_avail)) {
      m <- sprintf("%s is not one the current branches either in the local
                   repository or in any of the remote repositories. This will
                   create a new branch in the remote repository with this name
                   which will not have a corresponding local branch. This is
                   not a traditional setup, so this may not have been what you
                   intended.", branch)
      warning(wrap(m))
    }
  }

  # Obtain authentication ------------------------------------------------------

  # Determine if using HTTPS or SSH protocol
  if (remote %in% names(remote_avail)) {
    url <- remote_avail[remote]
  } else {
    url <- remote
  }
  if (stringr::str_sub(url, 1, 5) == "https") {
    protocol <- "https"
  } else if (stringr::str_sub(url, 1, 4) == "git@") {
    protocol <- "ssh"
  } else {
    m <- "The URL to the remote repository is using an unknown protocol. It
         should start with https if you are using your username and password
         for authentication, or with git@ if you are using your SSH keys. If
         you are trying to acheive something non-standard, please use Git
         via the command line interface."
    stop(wrap(m))
  }

  if (protocol == "https") {
    if (is.null(username)) {
      username <- readline("Please enter your GitHub username: ")
    }
    if (is.null(password)) {
      password <- getPass::getPass("Please enter your GitHub password: ")
    }
    credentials <- git2r::cred_user_pass(username = username,
                                         password = password)
  } else if (protocol == "ssh") {
    credentials <- git2r::cred_ssh_key()
  }

  # Push! ----------------------------------------------------------------------

  if (!dry_run) {
    tryCatch(git2r::push(r, name = remote,
                         refspec = paste0("refs/heads/", branch),
                         force = force, credentials = credentials),
             error = function(e) {
               if (stringr::str_detect(e$message, "unsupported URL protocol") &&
                   protocol == "ssh") {
                 reason <- "workflowr was unable to use your SSH keys. Run `git
                           push` in the Terminal instead."
               } else {
                 reason <- "Push failed for unknown reason."
               }
               stop(wrap(reason), call. = FALSE)
             }
    )
  }

  # Prepare output -------------------------------------------------------------

  o <- list(remote = remote, branch = branch, username = username,
            force = force, dry_run = dry_run)
  class(o) <- "wflow_push"
  return(o)
}

# Get the branches for a Git repository
#
# repo - git_repository object created with git2r
#
# Returns the union of the names of the local and remote branches for a Git repo
# as a character vector
get_branches <- function(repo) {
  if (class(repo) != "git_repository")
    stop("repo must be a git_repository object")

  b_local <- names(git2r::branches(repo, flags = "local"))
  b_remote <- names(git2r::branches(repo, flags = "remote"))
  b_remote <- stringr::str_split_fixed(b_remote, pattern = "/", n = 2)[, 2]
  b <- unique(c(b_local, b_remote))
  return(b)
}
