#' Pull files from remote repository
#'
#' \code{wflow_git_pull} pulls the remote files from your remote repository on
#' GitHub into your repository on your local machine. This is a convenience
#' function to run Git commands from the R console instead of the Terminal. The
#' same functionality can be acheived by running \code{git pull} in the
#' Terminal.
#'
#' \code{wflow_git_pull} tries to choose sensible defaults if the user does not
#' explicitly specify the remote repository and/or the remote branch:
#'
#' \itemize{
#'
#' \item If both \code{remote} and \code{branch} are \code{NULL},
#' \code{wflow_git_pull} checks to see if the current local branch is tracking a
#' remote branch. If yes, it pulls to this tracked remote branch.
#'
#' \item If the argument \code{remote} is left as \code{NULL} and there is only
#' one remote, it is used.  If there is more than one remote, the one named
#' "origin" is used.
#'
#' \item If the argument \code{branch} is left as \code{NULL}, the
#' name of the current local branch is used (referred to as \code{HEAD} by Git).
#'
#' }
#'
#' Under the hood, \code{wflow_git_pull} is a wrapper for \code{\link[git2r]{pull}}
#' from the package \link{git2r}.
#'
#' @param remote character (default: NULL). The name of the remote repository.
#'   See Details for the default behavior.
#' @param branch character (default: NULL). The name of the branch in the remote
#'   repository to pull from. If \code{NULL}, the name of the current local
#'   branch is used.
#' @param username character (default: NULL). GitHub username. The user is
#'   prompted if necessary.
#' @param password character (default: NULL). GitHub password. The user is
#'   prompted if necessary.
#' @param dry_run logical (default: FALSE). Preview the proposed action but do
#'   not actually pull from the remote repository.
#' @param project character (default: ".") By default the function assumes the
#'   current working directory is within the project. If this is not true,
#'   you'll need to provide the path to the project directory.
#'
#' @return An object of class \code{wflow_git_pull}, which is a list with the
#'   following elements:
#'
#' \itemize{
#'
#' \item \bold{remote}: The remote repository.
#'
#' \item \bold{branch}: The branch of the remote repository.
#'
#' \item \bold{username}: GitHub username.
#'
#' \item \bold{merge_result}: The \code{\link[git2r]{git_merge_result-class}}
#' object returned by \link{git2r} (only included if \code{dry_run == FALSE}).
#'
#' \item \bold{dry_run}: The input argument \code{dry_run}.
#'
#' }
#'
#' @examples
#' \dontrun{
#'
#' # Pull from remote repository
#' wflow_git_pull()
#' # Preview by running in dry run mode
#' wflow_git_pull(dry_run = TRUE)
#' }
#'
#' @export
wflow_git_pull <- function(remote = NULL, branch = NULL, username = NULL,
                           password = NULL, dry_run = FALSE, project = ".") {

  # Check input arguments ------------------------------------------------------

  if (!(is.null(remote) || (is.character(remote) && length(remote) == 1)))
    stop("remote must be NULL or a one-element character vector")

  if (!(is.null(branch) || (is.character(branch) && length(branch) == 1)))
    stop("branch must be NULL or a one-element character vector")

  if (!(is.null(username) || (is.character(username) && length(username) == 1)))
    stop("username must be NULL or a one-element character vector")

  if (!(is.null(password) || (is.character(password) && length(password) == 1)))
    stop("password must be NULL or a one-element character vector")

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
  git_head <- git2r::head(r)
  remote_avail <- wflow_remotes(verbose = FALSE, project = project)

  # Fail early if HEAD does not point to a branch
  if (!git2r::is_branch(git_head)) {
    m <-
      "You are not currently on any branch. Instead you are in 'detached HEAD'
      state. workflowr doesn't support such advanced Git options. If you
      didn't mean to do this, try running `git checkout master` in the
      Terminal. If you did mean to do this, please use Git directly from the
      Terminal to pull your commits."
    stop(wrap(m))
  }

  # Fail early if no remotes (and the remote argument isn't a URL)
  if (length(remote_avail) == 0) {
    if (is.null(remote)) {
      m <-
        "No remote repositories are available. Run ?wflow_remotes to learn how
        to configure this."
      stop(wrap(m))
    } else if ("https" %in% remote || "git@" %in% remote) {
      m <-
        "Instead of specifying the URL to the remote repository, you can save
        it as a remote. Run ?wflow_remotes for details."
      warning(wrap(m))
    } else {
      m <-
        "You have specifed a remote, but this remote repository has not
        remotes set. Run ?wflow_remotes to learn how to configure this."
      stop(wrap(m))
    }
  }

  # Fail early if remote is specified but doesn't exist
  if (!is.null(remote) && !(remote %in% names(remote_avail))) {
    m <-
      "The remote you specified is not one of the remotes available. Run
      ?wflow_remotes to learn how to add this remote."
    stop(wrap(m))
  }

  # Determine remote and branch ------------------------------------------------

  remote_and_branch <- determine_remote_and_branch(r, remote, branch)
  remote <- remote_and_branch$remote
  branch <- remote_and_branch$branch

  # Send warning if the remote branch is not the same one as local branch (HEAD)
  if (branch != git_head@name) {
    m <- sprintf(
      "The remote branch is \"%s\", but the current local branch is \"%s\".
      This is a valid option, but it is non-conventional. Is this what you
      intended?",
      branch, git_head@name)
    warning(wrap(m))
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

  if (protocol == "https" && !dry_run) {
    if (is.null(username)) {
      if (interactive()) {
        username <- readline("Please enter your GitHub username: ")
      } else {
       m <-
         "No username was specified. Either include the username in the
         function call or run the command in an interactive R session to be
         prompted to enter it."
       stop(wrap(m))
      }
    }
    if (is.null(password)) {
      if (interactive()) {
        password <- getPass::getPass("Please enter your GitHub password: ")
      } else {
        m <-
         "No password was specified. Either include the password in the
         function call (not recommended) or run the command in an interactive
         R session to be prompted to enter it in a secure manner."
        stop(wrap(m))
      }
    }
    credentials <- git2r::cred_user_pass(username = username,
                                         password = password)
  } else if (protocol == "ssh") {
    credentials <- git2r::cred_ssh_key()
  }

  # Pull! ----------------------------------------------------------------------

  # Do the pull in 2 steps: fetch+merge, b/c git2r::pull only allows pulling
  # from the tracked branch.
  if (!dry_run) {
    tryCatch(git2r::fetch(r, name = remote,
                          refspec = paste0("refs/heads/", branch),
                          credentials = credentials),
             error = function(e) {
               if (stringr::str_detect(e$message, "unsupported URL protocol") &&
                   protocol == "ssh") {
                 reason <- "workflowr was unable to use your SSH keys. Run `git
                           pull` in the Terminal instead."
               } else {
                 reason <- "Pull failed for unknown reason."
               }
               stop(wrap(reason), call. = FALSE)
             }
    )
    merge_result <- git2r::merge(r, paste(remote, branch, sep = "/"))
  } else {
    merge_result <- NULL
  }

  # Prepare output -------------------------------------------------------------

  o <- list(remote = remote, branch = branch, username = username,
            merge_result = merge_result, dry_run = dry_run)
  class(o) <- "wflow_git_pull"
  return(o)
}

#' @export
print.wflow_git_pull <- function(x, ...) {
  cat("Summary from wflow_git_pull\n\n")

  cat(wrap(sprintf(
    "Pulling from the branch \"%s\" of the remote repository \"%s\"",
    x$branch, x$remote)), "\n\n")

  if (x$dry_run) {
    cat("The following Git command would be run:\n\n")
  } else {
    cat("The following Git command was run:\n\n")
  }

  git_cmd <- "  $ git pull"
  git_cmd <- paste(git_cmd, x$remote, x$branch)
  cat(git_cmd, "\n")

  if (!is.null(x$merge_result)) {
    if (x$merge_result@up_to_date) {
      cat("\n", wrap(
        "No changes were made because your local and remote repositories are
        in sync."
        ), "\n", sep = "")
    } else if (x$merge_result@fast_forward && length(x$merge_result@sha) == 0) {
      cat("\n", wrap(
        "The latest changes in the remote repository were successfully pulled
        into your local repository."
      ), "\n", sep = "")
    } else if (x$merge_result@fast_forward) {
      cat("\n", wrap(sprintf(
        "The latest changes in the remote repository were successfully pulled
        into your local repository. To combine the changes that differed
        between the two repositories, the merge commit %s was created.",
      x$merge_result@sha)), "\n", sep = "")
    } else if (x$merge_result@conflicts) {
      cat("\n", wrap(
        "There were conflicts that Git could not resolve automatically when
        trying to pull changes from the remote repository. You will need to
        use Git from the Terminal to resolve these conflicts manually. Run
        `git status` in the Terminal to get started."
      ), "\n", sep = "")
    }
  }

  cat("\n")
  return(invisible(x))
}
