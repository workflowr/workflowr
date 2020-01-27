context("wflow_git_push_pull")

# Setup ------------------------------------------------------------------------

# Only newer tests use the setup functions
source("setup.R")

library("git2r")

# Setup workflowr project for testing
site_dir <- tempfile("test-wflow_git_push_pull-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE, user.name = "Test Name",
                             user.email = "test@email"))
# Delete workflowr project on exit
on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))
site_dir <- workflowr:::absolute(site_dir)
p <- wflow_paths(project = site_dir)
r <- repository(p$root)

# Test check_branch ------------------------------------------------------------

test_that("check_branch passes if HEAD points to a branch", {
  git_head <- git2r::repository_head(r)
  expect_silent(check_branch(git_head))
})

test_that("check_branch fails if HEAD does *not* point to a branch", {
  latest_commit <- commits(r)[[1]]
  checkout(latest_commit)
  on.exit(checkout(r, branch = "master"))
  git_head <- git2r::repository_head(r)
  expect_error(check_branch(git_head), "You are not currently on any branch")
})

# Test check_remote ------------------------------------------------------------

test_that("check_remote throws error if no remote repositories available", {
  expect_error(check_remote(NULL, character()),
               "No remote repositories are available")
  expect_error(check_remote("origin", character()),
               "No remote repositories are available")
  expect_error(check_remote("https://github.com/user/repo.git", character()),
               "No remote repositories are available.")
  expect_error(check_remote("git@github.com:user/repo.git", character()),
               "No remote repositories are available.")
})

# Add a remote
remote_avail <- wflow_git_remote("origin", "user", "repo", verbose = FALSE,
                              project = site_dir)

test_that("check_remote fails if remote is not one of the available repositories", {
  expect_error(check_remote("random", remote_avail),
               "The remote you specified is not one of the remotes available")
  expect_error(check_remote("https://github.com/user/repo.git", remote_avail),
               "The remote you specified is not one of the remotes available")
  expect_error(check_remote("git@github.com:user/repo.git", remote_avail),
               "The remote you specified is not one of the remotes available")
})

test_that("check_remote passes if remote is one of the available repositories", {
  expect_silent(check_remote("origin", remote_avail))
})

test_that("check_remote passes if remote is not specified but remotes are available", {
  expect_silent(check_remote(NULL, remote_avail))
})

# Test determine_remote_and_branch ---------------------------------------------

test_that("determine_remote_and_branch does nothing if both are specified", {
  result <- determine_remote_and_branch(r, remote = "x", branch = "y")
  expect_true(result$remote == "x")
  expect_true(result$branch == "y")
})

test_that("determine_remote_and_branch uses the name of the current branch if branch not specified", {
  result <- determine_remote_and_branch(r, remote = "x", branch = NULL)
  expect_true(result$branch == git2r::repository_head(r)$name)
  checkout(r, branch = "test", create = TRUE)
  on.exit(checkout(r, branch = "master"))
  result <- determine_remote_and_branch(r, remote = "x", branch = NULL)
  expect_true(result$branch == git2r::repository_head(r)$name)
})

test_that("determine_remote_and_branch uses the name of the current branch if branch not specified", {
  result <- determine_remote_and_branch(r, remote = "x", branch = NULL)
  expect_true(result$branch == git2r::repository_head(r)$name)
  checkout(r, branch = "test", create = TRUE)
  on.exit(checkout(r, branch = "master"))
  result <- determine_remote_and_branch(r, remote = "x", branch = NULL)
  expect_true(result$branch == git2r::repository_head(r)$name)
})

test_that("determine_remote_and_branch uses the only remote if it is not specified", {
  result <- determine_remote_and_branch(r, remote = NULL, branch = "y")
  expect_true(result$remote == names(remote_avail))
})

# Add a second remote
remote_avail <- wflow_git_remote("upstream", "user2", "repo", verbose = FALSE,
                              project = site_dir)

test_that("determine_remote_and_branch uses origin if it is not specified and there are more than one available", {
  result <- determine_remote_and_branch(r, remote = NULL, branch = "y")
  expect_true(result$remote == "origin")
})

# Add a third remote and remove origin
remote_avail <- wflow_git_remote("num3", "user3", "repo", verbose = FALSE,
                              project = site_dir)
remote_avail <- wflow_git_remote("origin", action = "remove", verbose = FALSE,
                              project = site_dir)

test_that("determine_remote_and_branch throws an error if remote not specified and multiple non-origin available", {
  expect_error(determine_remote_and_branch(r, remote = NULL, branch = "y"),
               "Unable to guess which remote repository to use")
})

# I tried setting a fake remote tracking branch to test, but I couldn't trick
# git2r into this. `branch_set_upstream` won't set a non-existing branch. It
# lets me set it if I first create a fake branch that looks like a remote branch
# (e.g. `branch_create(commit = commits(r)[[1]], "remote/another")`), but then
# `branch_get_upstream` fails because that's not actually a remote branch.

# Test warn_branch_mismatch ----------------------------------------------------

test_that("warn_branch_mismatch is silent if local and remote branch match", {
  expect_silent(warn_branch_mismatch("a", "a"))
})

test_that("warn_branch_mismatch warns if local and remote branch do *not* match", {
  expect_warning(warn_branch_mismatch("a", "b"),
               "The remote branch is \"a\", but the current local branch is \"b\".")
})

# Test get_remote_protocol -----------------------------------------------------

test_that("get_remote_protocol can detect HTTPS remote", {
  protocol <- get_remote_protocol(remote = "https://github.com/user/repo.git",
                                  remote_avail = remote_avail)
  expect_identical(protocol, "https")
})

test_that("get_remote_protocol can detect HTTPS remote by alias", {
  protocol <- get_remote_protocol(remote = "upstream",
                                  remote_avail = remote_avail)
  expect_identical(protocol, "https")
})

test_that("get_remote_protocol can detect SSH remote", {
  protocol <- get_remote_protocol(remote = "git@github.com:user/repo.git",
                                  remote_avail = remote_avail)
  expect_identical(protocol, "ssh")
})

test_that("get_remote_protocol fails for unknown protocol", {
  expect_error(get_remote_protocol(remote = "xyz:user/repo.git",
                                   remote_avail = remote_avail),
               "The URL to the remote repository is using an unknown protocol")
})

# Test authenticate_git --------------------------------------------------------

test_that("authenticate_git can create HTTPS credentials", {
  cred <- authenticate_git(protocol = "https",
                           username = "fakeuser", password = "fakepass")
  expect_true(inherits(cred, "cred_user_pass"))
  expect_true(cred$username == "fakeuser")
  expect_true(cred$password == "fakepass")
})

test_that("authenticate_git returns NULL for SSH remotes", {
  cred <- authenticate_git(protocol = "ssh")
  expect_true(is.null(cred))
})

test_that("authenticate_git only accepts https or ssh", {
  expect_error(authenticate_git(protocol = "xyz"))
})

test_that("authenticate_git fails if no username provided for https", {

  if (interactive()) skip("Interactive session")

  expect_error(authenticate_git(protocol = "https", password = "fakepass"),
               "No username was specified")
})

test_that("authenticate_git fails if no password provided for https", {

  if (interactive()) skip("Interactive session")

  expect_error(authenticate_git(protocol = "https", username = "fakeuser"),
               "No password was specified")
})

# Test wflow_git_push and wflow_git_pull ---------------------------------------

# Add back "origin"
wflow_git_remote("origin", "user", "repo", verbose = FALSE, project = site_dir)

test_that("wflow_git_push can run in dry-run mode", {
  expect_silent(result <- wflow_git_push(dry_run = TRUE, project = site_dir))
  expect_identical(result$remote, "origin")
  expect_identical(result$branch, "master")
  expect_identical(result$force, FALSE)
  expect_identical(result$dry_run, TRUE)
  expect_identical(result$protocol, "https")
  # Test print method
  expect_true("  $ git push origin master" %in% utils::capture.output(result))
  expect_true("Using the HTTPS protocol" %in% utils::capture.output(result))
})

test_that("wflow_git_pull can run in dry-run mode", {
  expect_silent(result <- wflow_git_pull(dry_run = TRUE, project = site_dir))
  expect_identical(result$remote, "origin")
  expect_identical(result$branch, "master")
  expect_identical(result$dry_run, TRUE)
  expect_identical(result$protocol, "https")
  # Test print method
  expect_true("  $ git pull origin master" %in% utils::capture.output(result))
  expect_true("Using the HTTPS protocol" %in% utils::capture.output(result))
})

test_that("wflow_git_push/pull fail early if try to use SSH protocol when not supported", {

  wflow_git_remote(remote = "testssh", user = "user", repo = "repo",
                   protocol = "ssh", project = site_dir)

  if (git2r::libgit2_features()$ssh) {
    expect_silent(wflow_git_push(remote = "testssh", dry_run = TRUE,
                                 project = site_dir))
    expect_silent(wflow_git_pull(remote = "testssh", dry_run = TRUE,
                                 project = site_dir))
  } else {
    expect_error(wflow_git_push(remote = "testssh", dry_run = TRUE,
                                project = site_dir),
                 "You cannot use the SSH protocol")
    expect_error(wflow_git_pull(remote = "testssh", dry_run = TRUE,
                                project = site_dir),
                 "You cannot use the SSH protocol")
  }

  wflow_git_remote(remote = "testssh", action = "remove", project = site_dir)
})

# Test print.wflow_git_pull ----------------------------------------------------

# Pass fake "wflow_git_pull" objects to see if it makes the right decisions

test_that("prints correctly from fast-forward merge", {
  m <- structure(list(
    up_to_date = FALSE,
    fast_forward = TRUE,
    conflicts = FALSE,
    sha = NA_character_),
    .Names = c("up_to_date", "fast_forward", "conflicts", "sha"),
    class = "git_merge_result")

  o <- list(remote = "remote", branch = "branch", username = "username",
            merge_result = m, dry_run = FALSE)
  class(o) <- "wflow_git_pull"

  expect_output(print(o), "fast-forward merge")
})

test_that("prints correctly from up_to_date merge", {
  m <- structure(list(
    up_to_date = TRUE,
    fast_forward = FALSE,
    conflicts = FALSE,
    sha = NA_character_),
    .Names = c("up_to_date", "fast_forward", "conflicts", "sha"),
    class = "git_merge_result")

  o <- list(remote = "remote", branch = "branch", username = "username",
            merge_result = m, dry_run = FALSE)
  class(o) <- "wflow_git_pull"

  expect_output(print(o), "No changes were made")
})

test_that("prints correctly from merge commit", {
  m <- structure(list(
    up_to_date = FALSE,
    fast_forward = FALSE,
    conflicts = FALSE,
    sha = "e03f3c3a307f1173eceb160293da6ad50e0d0962"),
    .Names = c("up_to_date", "fast_forward", "conflicts", "sha"),
    class = "git_merge_result")

  o <- list(remote = "remote", branch = "branch", username = "username",
            merge_result = m, dry_run = FALSE)
  class(o) <- "wflow_git_pull"

  expect_output(print(o), "To combine the changes")
})

test_that("prints correctly from merge conflict", {

  if (interactive()) skip("Avoid conflicted files prompt")

  # Merge conflict due to unknown cause (committed, staged, or unstaged changes).
  # Pull aborted.
  # fail=TRUE
  # conflicts=TRUE
  m <- structure(list(
    up_to_date = FALSE,
    fast_forward = FALSE,
    conflicts = TRUE,
    sha = NA_character_),
    .Names = c("up_to_date", "fast_forward", "conflicts", "sha"),
    class = "git_merge_result")

    o <- list(remote = "remote", branch = "branch", username = "username",
            merge_result = m, fail = TRUE, dry_run = FALSE)
  class(o) <- "wflow_git_pull"

  expect_output(print(o), "No changes were made")

  # Merge conflict due to committed changes
  # fail=FALSE
  # conflicts=TRUE
  # Note: project needs to point to an actual Git repo b/c it searches for
  # conflicted files
  path <- fs::file_temp()
  on.exit(fs::dir_delete(path))
  fs::dir_create(path)
  r <- git2r::init(path)
  m <- structure(list(
    up_to_date = FALSE,
    fast_forward = FALSE,
    conflicts = TRUE,
    sha = NA_character_),
    .Names = c("up_to_date", "fast_forward", "conflicts", "sha"),
    class = "git_merge_result")

    o <- list(remote = "remote", branch = "branch", username = "username",
            merge_result = m, fail = FALSE, dry_run = FALSE, project = path)
  class(o) <- "wflow_git_pull"

  expect_output(print(o), "Git from the Terminal")

  # Merge conflict due to unstaged or staged changes
  # fail=FALSE
  # conflicts=FALSE
  m <- structure(list(
    up_to_date = FALSE,
    fast_forward = FALSE,
    conflicts = FALSE,
    sha = NA_character_),
    .Names = c("up_to_date", "fast_forward", "conflicts", "sha"),
    class = "git_merge_result")

    o <- list(remote = "remote", branch = "branch", username = "username",
            merge_result = m, fail = FALSE, dry_run = FALSE)
  class(o) <- "wflow_git_pull"

  expect_output(print(o), "local changes")
})

# Test get_conflicted_files ----------------------------------------------------

test_that("find_conflicted_line returns first line with <<<", {

  lines <- c(
    "<<<<<<< HEAD",
    "master branch",
    "=======",
    "feature branch",
    ">>>>>>> feature"
  )
  expect_equal(workflowr:::find_conflicted_line(lines), 1)

  lines <- c("another line", lines)
  expect_equal(workflowr:::find_conflicted_line(lines), 2)

  # decoy
  lines <- c("doesn't begin with <<<<<<<", lines)
  expect_equal(workflowr:::find_conflicted_line(lines), 3)
})

test_that("find_conflicted_line returns NA if no conflicting line found", {
  expect_identical(workflowr:::find_conflicted_line(""), NA_integer_)

  lines <- letters
  expect_identical(workflowr:::find_conflicted_line(lines), NA_integer_)
})

test_that("get_conflicted_lines return first conflicting line for each file", {

  files <- fs::file_temp(pattern = as.character(0:5))
  on.exit(fs::file_delete(files))
  writer <- function(file, conflicted, total = 10) {
    lines <- letters[seq_len(total)]
    if (conflicted != 0) lines[conflicted] <- "<<<<<<<"
    writeLines(lines, con = file)
  }
  Map(writer, files, conflicted = 0:5)

  observed <- workflowr:::get_conflicted_lines(files)
  expect_equivalent(observed, c(NA_integer_, 1:5))
})

test_that("get_conflicted_files returns files with merge conflicts", {
  path <- test_setup()
  on.exit(test_teardown(path))

  r <- repository(path)
  f <- file.path(path, glue::glue("test-{1:3}.txt"))
  checkout(r, branch = "feature", create = TRUE)
  lapply(f, writeLines, text = "feature branch")
  add(r, f)
  commit(r, "commit on feature branch")
  checkout(r, "master")
  lapply(f, writeLines, text = "master branch")
  add(r, f)
  commit(r, "commit on master branch")
  m <- merge(r, "feature", fail = FALSE)
  expect_identical(m$conflicts, TRUE)

  conflicted_files <- workflowr:::get_conflicted_files(path)
  expect_identical(conflicted_files, f)

  # Requires manual test to see if prompt works and RStudio opens files
  skip("Manual test")

  conflicted_lines <- workflowr:::get_conflicted_lines(conflicted_files)
  open_files_rstudio(conflicted_files, conflicted_lines)

  m <- structure(list(
    up_to_date = FALSE,
    fast_forward = FALSE,
    conflicts = TRUE,
    sha = NA_character_),
    .Names = c("up_to_date", "fast_forward", "conflicts", "sha"),
    class = "git_merge_result")

  o <- list(remote = "remote", branch = "branch", username = "username",
            merge_result = m, fail = FALSE, dry_run = FALSE, project = path)
  class(o) <- "wflow_git_pull"
  print(o)
})
