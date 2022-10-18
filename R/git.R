#' Extract a commit from a Git repository
#'
#' \code{extract_commit} extracts the 7-digit SHA1 identifier and message for a
#' specified commit.
#'
#' @param path character. Specify the path to a directory that is a Git
#'   repository (or any subdirectory of the Git repository).
#' @param num numeric. The number of the commit to extract in reverse
#'   chronological order. In other words, 1 is the most recent commit, 2 is the
#'   second most recent commit, etc.
#'
#' @return A list with the named elements \code{sha1} and \code{message} (both
#'   characters). If a Git repository is not found at \code{path}, both are
#'   \code{NA}.
#'
#' @examples
#' \dontrun{
#' # Most recent commit
#' extract_commit(".", 1)
#' # Penultimate commit
#' extract_commit(".", 2)
#' }
#' @export
#' @keywords internal
extract_commit <- function(path, num) {
  stopifnot(fs::file_exists(path),
            is.numeric(num),
            num == trunc(num),
            num > 0)
  path <- absolute(path)
  if (!git2r::in_repository(path)) {
    return(list(sha1 = "NA", message = "NA"))
  }
  repo <- git2r::repository(path, discover = TRUE)
  git_log <- utils::capture.output(git2r::reflog(repo))
  total_commits <- length(git_log)
  if (total_commits == 0) {
    return(list(sha1 = "NA", message = "NA"))
  }
  if (num > total_commits) {
    stop(sprintf("Invalid search: %d. This repo only has %d commits.",
                 num, total_commits))
  }
  commit <- git_log[num]
  sha1 <- substr(commit, 2, 8)
  commit_message <- strsplit(commit, split = "commit: ")[[1]][2]
  return(list(sha1 = sha1, message = commit_message))
}

# Check for user.name and user.email in .gitconfig
#
# path character. Path to repository
#
# If unable to find user.name and user.email, stops the program.
check_git_config <- function(path, custom_message = "this function") {
  stopifnot(is.character(path))
  # Only look for local configuration file if the directory exists and it is a
  # Git repo
  if (fs::dir_exists(path)) {
    look_for_local <- git2r::in_repository(path)
  } else {
    look_for_local <- FALSE
  }

  # Determine if user.name and user.email are set
  if (look_for_local) {
    r <- git2r::repository(path, discover = TRUE)
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
    stop(wrap(
      "You must set your user.name and user.email for Git first to be able to
      run ", custom_message, ". To do this, run the following command in R,
      replacing the arguments with your name and email address:\n\n
      wflow_git_config(user.name = \"Your Name\", user.email = \"email@domain\")"),
      call. = FALSE)
  }
}

# Check for staged changes
#
# path character. Path to repository
#
# If staged changes are detected, stops the program.
check_staged_changes <- function(path, custom_message = "this function") {
  stopifnot(is.character(path))

  r <- git2r::repository(path, discover = TRUE)
  git_status <- git2r::status(r)

  if (length(git_status$staged) == 0) {
    return(invisible())
  } else {
    # Format files
    files_staged <- as.character(git_status$staged)
    files_staged <- file.path(git2r::workdir(r), files_staged)
    files_staged <- relative(files_staged)
    files_staged <- utils::capture.output(dput(files_staged))
    stop(wrap(
      "The Git repository has staged changes. You must decide if you want to
      commit these changes first before you run ", custom_message, ". To do
      this, run the following command in R:\n\n wflow_git_commit(",
      files_staged, ")"),
      call. = FALSE)
  }
}

# Obtain all the committed files in a Git repository at a given commit.
#
# repo - a git_repository object
#
# commit - NULL (default) or a git_commit object
#
# sysgit - character (default: `Sys.which("git")`) Path to system Git executable
#          used to obtain committed files via `git ls-files`. Cannot be used
#          with commit argument. To disable, set `git = ""`.
#
# The default is to use the head commit.
#
# Returns absolute paths.
get_committed_files <- function(repo, commit = NULL,
                                sysgit = getOption("workflowr.sysgit", default = "")) {
  stopifnot(inherits(repo, "git_repository"))
  stopifnot(is.null(commit) || inherits(commit, "git_commit"))

  n_commits <- length(git2r::commits(repo))
  if (n_commits == 0) {
    return(NA)
  }

  # If Git is available and don't need a specific commit, use `git ls-files`
  if (!is.null(sysgit) && !is.na(sysgit) && nchar(sysgit) > 0 && is.null(commit)) {
    cmd <- sprintf("%s -C %s ls-files", shQuote(sysgit),
                   shQuote(git2r::workdir(repo)))
    suppressWarnings(files <- system(cmd, intern = TRUE, ignore.stderr = TRUE))
    # Using Git is supposed to be a convenient speed increase. If it fails for
    # any reason (a failure adds an attribute "status"), just continue and use
    # git2r/libgit2.
    if (is.null(attr(files, which = "status", exact = TRUE))) {
      files <- absolute(file.path(git2r::workdir(repo), files))
      return(files)
    }
  }

  if (is.null(commit)) {
    commit <- git2r::lookup(repo, git2r::branch_target(git2r::repository_head(repo)))
  }

  tree <- git2r::tree(commit)
  files <- ls_files(tree)
  files <- absolute(file.path(git2r::workdir(repo), files))
  return(files)
}

# List all files in a given "git_tree" object.
ls_files <- function (tree) {
  tree_list <- as.list(tree)
  tree_df <- as.data.frame(tree)
  names(tree_list) <- tree_df$name
  files <- tree_df$name[tree_df$type == "blob"]
  dirs <- tree_df$name[tree_df$type == "tree"]
  out <- files
  # Recurisvely call ls_files on the "git_tree" objects corresponding to each
  # subdirectory
  for (dir in dirs) {
    tree_next <- tree_list[[dir]]
    out <- c(out, file.path(dir, ls_files(tree_next)))
  }
  return(out)
}

# Get the files that have been committed to the repository more recently than
# their corresponding HTML files.
#
# repo: git_repository object
# files: character vector of filenames
# outdir: directory with website files
# sysgit: path to system Git executable to run `git log -n 1` to obtain time of
# last commit
get_outdated_files <- function(repo, files, outdir = NULL,
                                sysgit = getOption("workflowr.sysgit", default = "")) {
  if (length(files) == 0) return(files)
  stopifnot(inherits(repo, "git_repository"))

  ext <- tools::file_ext(files)
  if (!all(grepl("[Rr]md", ext)))
    stop("Only R Markdown files are accepted.")
  # Corresponding HTML files
  html <- to_html(files, outdir = outdir)
  # For each source file, determine if it has been committed more recently than
  # its corresponding HTML
  out_of_date <- logical(length = length(files))

  # If Git is available, use it to run `git log -n 1`
  if (!is.null(sysgit) && !is.na(sysgit) && nchar(sysgit) > 0) {
    last_commit_time <- last_commit_time_sysgit
  } else {
    last_commit_time <- last_commit_time_git2r
  }

  for (i in seq_along(files)) {
    recent_source_time <- last_commit_time(repo, files[i], sysgit = sysgit)
    recent_html_time <- last_commit_time(repo, html[i], sysgit = sysgit)
    if (recent_source_time >= recent_html_time) {
      out_of_date[i] <- TRUE
    }
  }
  outdated <- files[out_of_date]
  return(outdated)
}

last_commit_time_git2r <- function(repo, fname, ...) {
  last_commit <- git2r::commits(repo, n = 1, path = fname)[[1]]
  last_commit_time <- last_commit$author$when$time
  return(last_commit_time)
}

last_commit_time_sysgit <- function(repo, fname, sysgit, ...) {
  cmd <- sprintf("%s -C %s log -n 1 --date=raw --format=%%ad -- %s",
                 shQuote(sysgit), shQuote(git2r::workdir(repo)), shQuote(fname))
  raw_git <- suppressWarnings(system(cmd, intern = TRUE, ignore.stderr = TRUE))
  # If it fails for any reason, fall back on git2r
  if (!is.null(attr(raw_git, which = "status", exact = TRUE))) {
    return(last_commit_time_git2r(repo, fname))
  }
  unix_git <- stringr::str_split(raw_git, "\\s")[[1]][1]
  return(as.numeric(unix_git))
}

# Obtain the files updated in a commit
#
# Obtain the files updated in a commit, similar to \code{git status --stat}, by
# running a diff between the trees pointed to by the commit and its parent
# commit.
#
# This only works for commits that have one parent commit. Thus it will fail for
# merge commits (two or more parents) or the initial root commit (zero parents).
# This uses `diff,git_tree`. See the source code at
# \url{https://github.com/ropensci/git2r/blob/89d916f17cb979b3cc21cbb5834755a2cf075f5f/R/diff.r#L314}
# and examples at
# \url{https://github.com/ropensci/git2r/blob/cb30b1dd5f8b57978101ea7b7dc26ae2c9eed38e/tests/diff.R#L88}.
#
# @seealso \code{\link{obtain_files_in_commit_root}}
#
# Returns absolute paths.
obtain_files_in_commit <- function(repo, commit) {
  stopifnot(inherits(repo, "git_repository"),
            inherits(commit, "git_commit"))
  parent_commit <- git2r::parents(commit)

  # 3 possibilities:
  #
  # 1. Root commit with 0 parents
  # 2. Standard commit with 1 parent
  # 3. Merge commit with 2+ parents (yes, it's possible to merge more than 2 branches!)
  if (length(parent_commit) == 0) {
    files <- obtain_files_in_commit_root(repo, commit)
  } else if (length(parent_commit) == 1) {
    git_diff <- base::diff(git2r::tree(commit),
                            git2r::tree(parent_commit[[1]]))
    files <- sapply(git_diff$files,
                    function(x) x$new_file)
  } else {
    stop(sprintf("Cannot perform diff on commit %s because it has %d parents",
                 commit$sha, length(parent_commit)))
  }

  files <- absolute(file.path(git2r::workdir(repo), files))
  return(files)
}

# Obtain the files updated in the root commit
#
# The files included in the root commit cannot be determined comparing two
# trees (which is how \code{\link{obtain_files_in_commit}} works). See
# \href{https://stackoverflow.com/questions/41433034/how-to-obtain-files-included-in-initial-commit-using-git2r-libgit2}{this
# Stack Overflow question} for details.
#
# This only works for the root commit, i.e. it must have no parents.
#
# @seealso \code{\link{obtain_files_in_commit}}
#
# Returns paths relative to Git root directory.
obtain_files_in_commit_root <- function(repo, commit) {
  # Obtain the files in the root commit of a Git repository
  stopifnot(inherits(repo, "git_repository"),
            inherits(commit, "git_commit"),
            length(git2r::parents(commit)) == 0)
  entries <- as.data.frame(git2r::tree(commit))
  files <- character()
  while (nrow(entries) > 0) {
    if (entries$type[1] == "blob") {
      # If the entry is a blob, i.e. file:
      #  - record the name of the file
      #  - remove the entry
      files <- c(files, entries$name[1])
      entries <- entries[-1, ]
    } else if (entries$type[1] == "tree") {
      # If the entry is a tree, i.e. subdirectory:
      #  - lookup the entries for this tree
      #  - add the subdirectory to the name so that path is correct
      #  - remove the entry from beginning and add new entries to end of
      #    data.frame
      new_tree_df <- as.data.frame(git2r::lookup(repo, entries$sha[1]))
      new_tree_df$name <- file.path(entries$name[1], new_tree_df$name)
      entries <- rbind(entries[-1, ], new_tree_df)
    } else {
      stop(sprintf("Unknown type %s found in commit %s",
                   entries$type[1], commit))
    }
  }

  return(files)
}

# Stop if HEAD does not point to a branch
check_branch <- function(git_head) {
  if (!git2r::is_branch(git_head)) {
    m <-
      "You are not currently on any branch. Instead you are in 'detached HEAD'
      state. workflowr doesn't support such advanced Git options. If you
      didn't mean to do this, try running `git checkout master` in the
      Terminal. If you did mean to do this, please use Git directly from the
      Terminal to push your commits."
    stop(wrap(m), call. = FALSE)
  }
}

# Check remote repository.
#
# If there are no remotes available, throw an error.
#
# If a remote is specified, confirm it exists.
#
# remote - character vector or NULL
# remote_avail - a named character vector of remote URLs
check_remote <- function(remote, remote_avail) {

  if (!(is.null(remote) || is.character(remote)))
    stop("remote must be NULL or character vector")

  if (!is.character(remote_avail))
    stop("remote_avail must be a character vector")

  # If there are no remotes available, throw an error.
  if (length(remote_avail) == 0) {
    m <- "No remote repositories are available. Run ?wflow_git_remote to learn
          how to configure this."
    stop(wrap(m), call. = FALSE)
  }

  # Fail early if remote is specified but doesn't exist
  if (!is.null(remote) && !(remote %in% names(remote_avail))) {
    m <-
      "The remote you specified is not one of the remotes available. Run
      ?wflow_git_remote to learn how to add this remote."
    stop(wrap(m), call. = FALSE)
  }
}

# Determine which remote and branch to push or pull.
#
# This function assumes error handling has already happened upstream.
#
# See the documentation for wflow_git_push or wflow_git_pull for the explanation
# of this function.
#
# Returns a list of length two.
determine_remote_and_branch <- function(repo, remote, branch) {
  stopifnot(inherits(repo, "git_repository"))
  git_head <- git2r::repository_head(repo)
  tracking <- git2r::branch_get_upstream(git_head)
  # If both remote and branch are NULL and the current branch is tracking a
  # remote branch, use this remote and branch.
  if (is.null(remote) && is.null(branch) && !is.null(tracking)) {
    remote <- git2r::branch_remote_name(tracking)
    branch <- stringr::str_split_fixed(tracking$name,
                                       "/", n = 2)[, 2]
  }
  # If remote is NULL, take an educated guess at what the user would want.
  if (is.null(remote)) {
    remote <- guess_remote(repo)
  }
  # If branch is NULL, use the same name as the current branch.
  if (is.null(branch)) {
    branch <- git_head$name
  }

  return(list(remote = remote, branch = branch))
}

# Take an educated guess of which remote to use if the user didn't specify one
# and the current branch is not tracking a remote branch.
#
# 1. If there is only 1 remote available, use it.
# 2. If there are multiple remotes available and one is called "origin", use it.
# 3. If there are multiple remotes available and none is "origin", throw error.
guess_remote <- function(repo) {
  stopifnot(inherits(repo, "git_repository"))
  remotes <- git2r::remotes(repo)

  if (length(remotes) == 1) {
    guess <- remotes
  } else if ("origin" %in% remotes) {
    guess <- "origin"
  } else {
    m <-
      "Unable to guess which remote repository to use. Please specify the
      argument `remote`. To see all the remotes available, you can run
      `wflow_git_remote()`."
    stop(wrap(m), call. = FALSE)
  }

  return(guess)
}

# Send warning if the remote branch is not the same one as local branch (HEAD)
warn_branch_mismatch <- function(remote_branch, local_branch) {
  if (!(is.character(remote_branch) && is.character(local_branch)))
    stop("remote_branch and local_branch must be character vectors")

  if (remote_branch != local_branch) {
    m <- sprintf(
      "The remote branch is \"%s\", but the current local branch is \"%s\".
      This is a valid option, but it is non-conventional. Is this what you
      intended?",
      remote_branch, local_branch)
    warning(wrap(m), call. = FALSE)
  }
}

# Determine if using HTTPS or SSH protocol
#
# remote - the name or URL of a remote repository. Note: The upstream function
# wflow_git_push()/pull() no longer accept direct URLs to a remote repository.
# However, I'm leaving this functionality in this function since it doesn't hurt
# anything and could be potentially useful in the future.
#
# remote_avail - a named character vector of remote URLs
#
# Return either "https" or "ssh"
get_remote_protocol <- function(remote, remote_avail) {
  if (!(is.character(remote) && is.character(remote_avail)))
    stop("remote and remote_avail must be character vectors")

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
    stop(wrap(m), call. = FALSE)
  }

  return(protocol)
}

# Authenticate with Git using either HTTPS or SSH
#
# protocol - either "https" or "ssh"
# username - username or NULL
# password - password or NULL
# dry_run - logical
authenticate_git <- function(protocol, username = NULL,
                             password = NULL, dry_run = FALSE) {
  if (!protocol %in% c("https", "ssh"))
    stop("protocol must be either \"https\" or \"ssh\"")
  if (!(is.null(username) || (is.character(username) && length(username) == 1)))
    stop("username must be NULL or a one-element character vector")
  if (!(is.null(password) || (is.character(password) && length(password) == 1)))
    stop("password must be NULL or a one-element character vector")

  if (protocol == "https" && !dry_run) {
    if (is.null(username)) {
      if (interactive()) {
        response <- ""
        while (response == "") {
          response <- readline("Please enter your username (Esc to cancel): ")
        }
        username <- response
      } else {
        m <-
          "No username was specified. Either include the username in the
         function call or run the command in an interactive R session to be
         prompted to enter it."
        stop(wrap(m), call. = FALSE)
      }
    }
    if (is.null(password)) {
      if (interactive()) {
        password <- getPass::getPass("Please enter your password: ")
      } else {
        m <-
          "No password was specified. Either include the password in the
         function call (not recommended) or run the command in an interactive
         R session to be prompted to enter it in a secure manner."
        stop(wrap(m), call. = FALSE)
      }
    }
    credentials <- git2r::cred_user_pass(username = username,
                                         password = password)
  } else {
    # If dry run, credentials aren't needed.
    #
    # If using SSH, can't run cred_ssh_key() here if using a passphrase.
    # credentials has to be entered as NULL when calling push or pull in order
    # for it to work.
    #
    # https://github.com/hadley/devtools/issues/642#issuecomment-139357055
    # https://github.com/ropensci/git2r/issues/284#issuecomment-306103004
    credentials <- NULL
  }
  return(credentials)
}

# Throw error if Git repository is locked
check_git_lock <- function(r) {
  stopifnot(inherits(r, "git_repository"))

  index_lock <- file.path(git2r::workdir(r), ".git/index.lock")
  if (fs::file_exists(index_lock)) {
    stop(call. = FALSE, wrap(
      "The Git repository is locked. This can happen if a Git command
      previously crashed or if multiple Git commands were executed at the same
      time. To fix this, you need to delete the file .git/index.lock. You can
      do this by running the following in the R console:"),
      "\n\n",
      glue::glue("file.remove(\"{index_lock}\")")
    )
  }
}
