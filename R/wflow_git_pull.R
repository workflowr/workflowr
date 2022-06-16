#' Pull files from remote repository
#'
#' \code{wflow_git_pull} pulls the remote files from your remote repository
#' online (e.g. GitHub or GitLab) into your repository on your local machine.
#' This is a convenience function to run Git commands from the R console instead
#' of the Terminal. The same functionality can be achieved by running \code{git
#' pull} in the Terminal.
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
#' Under the hood, \code{wflow_git_pull} is a wrapper for
#' \code{\link[git2r]{pull}} from the package \link{git2r}.
#'
#' @param remote character (default: NULL). The name of the remote repository.
#'   See Details for the default behavior.
#' @param branch character (default: NULL). The name of the branch in the remote
#'   repository to pull from. If \code{NULL}, the name of the current local
#'   branch is used.
#' @param username character (default: NULL). Username for online Git hosting
#'   service (e.g. GitHub or GitLab). The user is prompted if necessary.
#' @param password character (default: NULL). Password for online Git hosting
#'   service (e.g. GitHub or GitLab). The user is prompted if necessary.
#' @param fail logical (default: TRUE) Abort the pull if any merge conflicts
#'   are detected. If you are sure you want to manually cleanup the merge
#'   conflicts, set \code{fail = FALSE}. The argument \code{fail} is passed to
#'   the git2r function \code{\link[git2r:reexports]{merge.git_repository}}.
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
#' \item \bold{username}: Username for online Git hosting service (e.g. GitHub
#' or GitLab).
#'
#' \item \bold{merge_result}: The \code{git_merge_result} object returned by
#' \link{git2r} (only included if \code{dry_run == FALSE}).
#'
#' \item \bold{fail}: The input argument \code{fail}.
#'
#' \item \bold{dry_run}: The input argument \code{dry_run}.
#'
#' \item \bold{protocol}: The authentication protocol for the remote repository
#' (either \code{"https"} or \code{"ssh"}.
#'
#' \item \bold{project}: The input argument \code{project}.
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
                           password = NULL, fail = TRUE, dry_run = FALSE,
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

  assert_is_flag(fail)
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

  # Pull! ----------------------------------------------------------------------

  # Do the pull in 2 steps: fetch+merge, b/c git2r::pull only allows pulling
  # from the tracked branch.
  git_alternative <- glue::glue("
    Alternatively, if you have Git installed on your machine, the easiest
    solution is to instead run `git pull` in the terminal. This is equivalent
    to wflow_git_pull(). Specifically, copy-paste the following in the
    terminal:

    git pull {remote} {branch}
    ")
  if (!dry_run) {
    tryCatch(git2r::fetch(r, name = remote,
                          refspec = paste0("refs/heads/", branch),
                          credentials = credentials),
             error = function(e) {
               if (protocol == "ssh" &&
                   stringr::str_detect(conditionMessage(e), "unsupported URL protocol")) {
                 reason <-
                   "workflowr was unable to use your SSH keys because your
                   computer does not have the required software installed. If
                   you want to be able to pull directly from R, re-install the
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
               } else {
                 reason <- c("Pull failed for unknown reason.",
                             "\n\nThe error message from git2r::pull() was:\n\n",
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
    merge_result <- git2r_merge(r, paste(remote, branch, sep = "/"), fail = fail)
  } else {
    merge_result <- NULL
  }

  # Prepare output -------------------------------------------------------------

  o <- list(remote = remote, branch = branch, username = username,
            merge_result = merge_result, fail = fail, dry_run = dry_run,
            protocol = protocol, project = project)
  class(o) <- "wflow_git_pull"
  return(o)
}

#' @export
print.wflow_git_pull <- function(x, ...) {
  cat("Summary from wflow_git_pull\n\n")

  cat(wrap(sprintf(
    "Pulling from the branch \"%s\" of the remote repository \"%s\"",
    x$branch, x$remote)), "\n\n")

  cat(glue::glue("Using the {toupper(x$protocol)} protocol\n\n"))

  if (x$dry_run) {
    cat("The following Git command would be run:\n\n")
  } else {
    cat("The following Git command was run:\n\n")
  }

  git_cmd <- "  $ git pull"
  git_cmd <- paste(git_cmd, x$remote, x$branch)
  cat(git_cmd)
  cat("\n")

  # Note: Use "exit early" strategy instead of nested if-else clauses
  if (is.null(x$merge_result)) return(invisible(x))

  if (x$merge_result$up_to_date) {
    m <- "No changes were made because your local and remote repositories are in sync."
    cat("\n", wrap(m), "\n", sep = "")
    cat("\n")
    return(invisible(x))
  }

  if (x$merge_result$fast_forward) {
    m <- "The latest changes in the remote repository were successfully pulled
          into your local repository (fast-forward merge)."
    cat("\n", wrap(m), "\n", sep = "")
    cat("\n")
    return(invisible(x))
  }

  if (!is.na(x$merge_result$sha)) {
    m <- sprintf(
      "The latest changes in the remote repository were successfully pulled
        into your local repository. To combine the changes that differed
        between the two repositories, the merge commit %s was created.",
      x$merge_result$sha)
    cat("\n", wrap(m), "\n", sep = "")
    cat("\n")
    return(invisible(x))
  }

  # At this point, there must have been a merge conflict of some sort
  m <- "There were conflicts that Git could not resolve automatically when
       trying to pull changes from the remote repository."
  cat("\n", wrap(m), "\n", sep = "")
  cat("\n")

  if (x$fail) {
    m <- "No changes were made to your files because workflowr aborted the pull.
         Try cleaning up your files by committing the changes you want and
         discarding those you don't. To allow workflowr to proceed with the pull
         and potentially generate merge conflicts, re-run wflow_git_pull() with
         the argument fail=FALSE."
    cat("\n", wrap(m), "\n", sep = "")
    cat("\n")
    return(invisible(x))
  }

  # Merge conflicts from committed changes. Merge conflicts are now unstaged changes
  if (x$merge_result$conflicts) {
    conflicted_files <- get_conflicted_files(x$project)
    cat("\nThe following file(s) contain conflicts:\n")
    cat(conflicted_files, sep = "\n")
    if (interactive() && rstudioapi::isAvailable(version_needed = "0.99.719")) {
      ans <- ""
      while(!tolower(ans) %in% c("y", "n")) {
        ans <- readline("Do you want workflowr to open the conflicting files in RStudio? (y/n) ")
      }
      if (tolower(ans) == "y") {
        conflicted_lines <- get_conflicted_lines(conflicted_files)
        open_files_rstudio(conflicted_files, conflicted_lines)
      }
    }
    m <- "You will need to use Git from the Terminal to resolve these conflicts
          manually. Run `git status` in the Terminal to get started."
    cat("\n", wrap(m), "\n", sep = "")
    cat("\n")
    return(invisible(x))
  }

  # Merge conflicts from unstaged or staged changes. Need to clean up repo first.
  m <- "The pull **failed** because you have made local changes to your files
         that would be overwritten by pulling the latest versions of the files.
         You need to first commit or discard these changes and then pull
         again."
  cat("\n", wrap(m), "\n", sep = "")
  cat("\n")
  return(invisible(x))
}

# Return conflicted files in a Git repository
get_conflicted_files <- function(path) {
  r <- git2r::repository(path)
  s <- git2r::status(r)
  s_df <- status_to_df(s)
  conflicted <- s_df[s_df$substatus == "conflicted", "file"]

  if (length(conflicted) == 0) return(NA)

  conflicted <- file.path(git2r::workdir(r), conflicted)
  return(conflicted)
}

get_conflicted_lines <- function(files) {
  list_of_lines <- Map(readLines, files)
  conflicted_lines <- Map(find_conflicted_line, list_of_lines)
  conflicted_lines <- unlist(conflicted_lines)
  return(conflicted_lines)
}

find_conflicted_line <- function(lines) {
  stringr::str_which(lines, "^<<<")[1]
}

open_files_rstudio <- function(files, lines = -1L) {
  mapply(rstudioapi::navigateToFile, file = files, line = lines)
  return(invisible(files))
}
