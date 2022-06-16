#' Push files to remote repository
#'
#' \code{wflow_git_push} pushes the local files on your machine to your remote
#' repository on a remote Git hosting service (e.g. GitHub or GitLab). This is a
#' convenience function to run Git commands from the R console instead of the
#' Terminal. The same functionality can be achieved by running \code{git push}
#' in the Terminal.
#'
#' \code{wflow_git_push} tries to choose sensible defaults if the user does not
#' explicitly specify the remote repository and/or the remote branch:
#'
#' \itemize{
#'
#' \item If both \code{remote} and \code{branch} are \code{NULL},
#' \code{wflow_git_push} checks to see if the current local branch is tracking a
#' remote branch. If yes, it pushes to this tracked remote branch.
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
#' Under the hood, \code{wflow_git_push} is a wrapper for \code{\link[git2r]{push}}
#' from the package \link{git2r}.
#'
#' @param remote character (default: NULL). The name of the remote repository.
#'   See Details for the default behavior.
#' @param branch character (default: NULL). The name of the branch to push to in
#'   the remote repository. If \code{NULL}, the name of the current local branch
#'   is used.
#' @param username character (default: NULL). Username for online Git hosting
#'   service (e.g. GitHub or GitLab). The user is prompted if necessary.
#' @param password character (default: NULL). Password for online Git hosting
#'   service (e.g. GitHub or GitLab). The user is prompted if necessary.
#' @param force logical (default: FALSE). Force the push to the remote
#'   repository. Do not use this if you are not 100\% sure of what it is doing.
#'   Equivalent to: \code{git push -f}
#' @param set_upstream logical (default: TRUE). Set the current local branch to
#'   track the remote branch if it isn't already tracking one. This is likely
#'   what you want. Equivalent to: \code{git push -u remote branch}
#' @param view logical (default: \code{getOption("workflowr.view")}). Open the
#'   URL to the repository in the browser. Ignored if \code{dry_run = TRUE}.
#'   Also note that this only works if the option \code{browser} is set, which
#'   you can check with \code{getOption("browser")}.
#' @param dry_run logical (default: FALSE). Preview the proposed action but do
#'   not actually push to the remote repository.
#' @param project character (default: ".") By default the function assumes the
#'   current working directory is within the project. If this is not true,
#'   you'll need to provide the path to the project directory.
#'
#' @return An object of class \code{wflow_git_push}, which is a list with the
#'   following elements:
#'
#' \itemize{
#'
#' \item \bold{remote}: The remote repository.
#'
#' \item \bold{branch}: The branch of the remote repository.
#'
#' \item \bold{username}: Username for online Git hosting service (e.g. GitHub
#' or GitLab).
#'
#' \item \bold{force}: The input argument \code{force}.
#'
#' \item \bold{set_upstream}: The input argument \code{set_upstream}.
#'
#' \item \bold{view}: The input argument \code{view}.
#'
#' \item \bold{dry_run}: The input argument \code{dry_run}.
#'
#' \item \bold{protocol}: The authentication protocol for the remote repository
#' (either \code{"https"} or \code{"ssh"}.
#'
#' }
#'
#' @examples
#' \dontrun{
#'
#' # Push to remote repository
#' wflow_git_push()
#' # Preview by running in dry run mode
#' wflow_git_push(dry_run = TRUE)
#' }
#'
#' @export
wflow_git_push <- function(remote = NULL, branch = NULL, username = NULL,
                           password = NULL, force = FALSE, set_upstream = TRUE,
                           view = getOption("workflowr.view"), dry_run = FALSE,
                           project = ".") {

  # Check input arguments ------------------------------------------------------

  if (!(is.null(remote) || (is.character(remote) && length(remote) == 1)))
    stop("remote must be NULL or a one-element character vector")

  if (!(is.null(branch) || (is.character(branch) && length(branch) == 1)))
    stop("branch must be NULL or a one-element character vector")

  if (!(is.null(username) || (is.character(username) && length(username) == 1)))
    stop("username must be NULL or a one-element character vector")

  if (!(is.null(password) || (is.character(password) && length(password) == 1)))
    stop("password must be NULL or a one-element character vector")

  assert_is_flag(force)
  assert_is_flag(set_upstream)
  assert_is_flag(view)
  assert_is_flag(dry_run)
  check_wd_exists()
  assert_is_single_directory(project)
  project <- absolute(project)

  # Assess status of repository ------------------------------------------------

  # Must be using Git
  p <- wflow_paths(error_git = TRUE, project = project)
  r <- git2r::repository(path = p$git)
  git_head <- git2r::repository_head(r)
  remote_avail <- wflow_git_remote(verbose = FALSE, project = project)

  # Fail early if HEAD does not point to a branch
  check_branch(git_head)

  # Fail early if remote not specified properly
  check_remote(remote = remote, remote_avail = remote_avail)

  # Determine remote and branch ------------------------------------------------

  remote_and_branch <- determine_remote_and_branch(r, remote, branch)
  remote <- remote_and_branch$remote
  branch <- remote_and_branch$branch

  # Send warning if the remote branch is not the same one as local branch (HEAD)
  warn_branch_mismatch(remote_branch = branch,
                       local_branch = git_head$name)

  # Determine protocol ---------------------------------------------------------

  protocol <- get_remote_protocol(remote = remote, remote_avail = remote_avail)

  if (protocol == "ssh" && !git2r::libgit2_features()$ssh) {
    stop(wrap(
      "You cannot use the SSH protocol for authentication on this machine because
      git2r/libgit2 was not built with SSH support. You can either switch to
      using the HTTPS protocol for authentication (see ?wflow_git_remote) or
      re-install git2r after installing libSSH2."),
      "\n\nFrom the git2r documentation:\n\n",
      "To build with SSH support, please install:\n",
      "  libssh2-1-dev (package on e.g. Debian and Ubuntu)\n",
      "  libssh2-devel (package on e.g. Fedora, CentOS and RHEL)\n",
      "  libssh2 (Homebrew package on OS X)"
      , call. = FALSE)
  }

  # Obtain authentication ------------------------------------------------------

  credentials <- authenticate_git(protocol = protocol,
                                  username = username, password = password,
                                  dry_run = dry_run)

  # Push! ----------------------------------------------------------------------

  if (!dry_run) {
    # First check for and execute any pre-push hooks. libgit2 does not support
    # this. Only supported on unix-alike systems.
    pre_push_file <- file.path(git2r::workdir(r), ".git/hooks/pre-push")
    pre_push_file_rel <- fs::path_rel(pre_push_file, start = getwd())
    if (fs::file_exists(pre_push_file) && .Platform$OS.type != "windows") {
      message(glue::glue("Executing pre-push hook in {pre_push_file_rel}"))
      hook <- suppressWarnings(system(pre_push_file, intern = TRUE))
      message(hook)
      if (attributes(hook)$status != 0) {
        stop(glue::glue("Execution stopped by {pre_push_file_rel}"),
             call. = FALSE)
      }
    }

    git_alternative <- glue::glue("
      Alternatively, if you have Git installed on your machine, the easiest
      solution is to instead run `git push` in the terminal. This is
      equivalent to wflow_git_push(). Specifically, copy-paste the following
      in the terminal:

      git push -u {remote} {branch}
      ")
    tryCatch(git2r::push(r, name = remote,
                         refspec = paste0("refs/heads/", branch),
                         force = force, credentials = credentials),
             error = function(e) {
               if (protocol == "ssh" &&
                   stringr::str_detect(conditionMessage(e), "unsupported URL protocol")) {
                 reason <-
                   "workflowr was unable to use your SSH keys because your
                   computer does not have the required software installed. If
                   you want to be able to push directly from R, re-install the
                   package git2r and follow its advice for how to enable SSH
                   for your operating system."
                 reason <- c(reason, "\n\n", git_alternative)
               } else if (protocol == "ssh" &&
                          stringr::str_detect(conditionMessage(e), "Failed to authenticate SSH session")) {
                 reason <-
                   "workflowr was unable to use your SSH keys because it has a
                   passphrase. You'll need to activate ssh-agent and add your
                   keys."
                 reason <- c(reason, "\n\n", git_alternative)
               } else if (stringr::str_detect(conditionMessage(e), "remote contains commits that are not present locally")) {
                 reason <-
                   "workflowr was unable to push because the remote repository
                   contains changes that are not present in your local
                   repository. Run wflow_git_pull() first to pull down these
                   changes to your local computer."
               } else {
                 reason <- c("Push failed for unknown reason.",
                             "\n\nThe error message from git2r::push() was:\n\n",
                             conditionMessage(e),
                             "\n\nThese sorts of errors are difficult to
                             troubleshoot. You can search for similar errors
                             on the git2r GitHub repository for advice on how
                             to fix it.")
                 reason <- c(reason, "\n\n", git_alternative)
               }
               stop(wrap(reason), call. = FALSE)
             }
    )
    # Set upstream tracking branch if it doesn't exist and `set_upstream=TRUE`
    local_branch_object <- git2r::repository_head(r)
    if (is.null(git2r::branch_get_upstream(local_branch_object)) && set_upstream) {
      git2r::branch_set_upstream(branch = local_branch_object,
                                 name = paste(remote, branch, sep = "/"))
    }
  }

  # Prepare output -------------------------------------------------------------

  o <- list(remote = remote, branch = branch, username = username,
            force = force, set_upstream = set_upstream, view = view,
            dry_run = dry_run, protocol = protocol)
  class(o) <- "wflow_git_push"

  browser <- check_browser()
  if (view && browser && !dry_run) {
    remote_url <- remote_avail[remote]
    # Remove trailing .git
    remote_url <- stringr::str_replace(remote_url, "\\.git$", "")
    # If SSH, replace with HTTPS URL
    remote_url <- stringr::str_replace(remote_url, "^git@(.+):", "https://\\1/")
    utils::browseURL(remote_url)
  }

  return(o)
}

#' @export
print.wflow_git_push <- function(x, ...) {
  cat("Summary from wflow_git_push\n\n")

  cat(wrap(sprintf(
    "Pushing to the branch \"%s\" of the remote repository \"%s\"",
    x$branch, x$remote)), "\n\n")

  cat(glue::glue("Using the {toupper(x$protocol)} protocol\n\n"))

  if (x$dry_run) {
    cat("The following Git command would be run:\n\n")
  } else {
    cat("The following Git command was run:\n\n")
  }
  if (x$force) {
    git_cmd <- "  $ git push -f"
  } else {
    git_cmd <- "  $ git push"
  }
  git_cmd <- paste(git_cmd, x$remote, x$branch)
  cat(git_cmd)

  cat("\n")
  return(invisible(x))
}
