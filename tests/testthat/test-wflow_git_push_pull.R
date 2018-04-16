context("wflow_git_push_pull")

# Setup ------------------------------------------------------------------------

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
  git_head <- head(r)
  expect_silent(check_branch(git_head))
})

test_that("check_branch fails if HEAD does *not* point to a branch", {
  latest_commit <- commits(r)[[1]]
  checkout(latest_commit)
  on.exit(checkout(r, branch = "master"))
  git_head <- head(r)
  expect_error(check_branch(git_head), "You are not currently on any branch")
})

# Test check_remote ------------------------------------------------------------

test_that("check_remote sends warning if remote is HTTPS URL", {
  expect_warning(check_remote("https://github.com/user/repo.git", character()),
                 "Instead of specifying the URL to the remote repository")
})

test_that("check_remote sends warning if remote is SSH URL", {
  expect_warning(check_remote("git@github.com:user/repo.git", character()),
                 "Instead of specifying the URL to the remote repository")
})

test_that("check_remote fails if remote not specified and no remote repositories", {
  expect_error(check_remote(NULL, character()),
               "No remote repositories are available")
})

test_that("check_remote fails if remote is specified and no remote repositories", {
  expect_error(check_remote("origin", character()),
               "You have specifed a remote")
})

# Add a remote
remote_avail <- wflow_git_remote("origin", "user", "repo", verbose = FALSE,
                              project = site_dir)

test_that("check_remote fails if remote is not one of the available repositories", {
  expect_error(check_remote("random", remote_avail),
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
  expect_true(result$branch == head(r)@name)
  checkout(r, branch = "test", create = TRUE)
  on.exit(checkout(r, branch = "master"))
  result <- determine_remote_and_branch(r, remote = "x", branch = NULL)
  expect_true(result$branch == head(r)@name)
})

test_that("determine_remote_and_branch uses the name of the current branch if branch not specified", {
  result <- determine_remote_and_branch(r, remote = "x", branch = NULL)
  expect_true(result$branch == head(r)@name)
  checkout(r, branch = "test", create = TRUE)
  on.exit(checkout(r, branch = "master"))
  result <- determine_remote_and_branch(r, remote = "x", branch = NULL)
  expect_true(result$branch == head(r)@name)
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

# Test authenticate_git --------------------------------------------------------

test_that("authenticate_git can create HTTPS credentials", {
  cred <- authenticate_git(remote = "upstream", remote_avail = remote_avail,
                           username = "fakeuser", password = "fakepass")
  expect_true(class(cred) == "cred_user_pass")
  expect_true(cred@username == "fakeuser")
  expect_true(cred@password == "fakepass")
})

test_that("authenticate_git returns NULL for SSH remotes", {
  cred <- authenticate_git(remote = "git@github.com:user/repo.git",
                           remote_avail = remote_avail)
  expect_true(is.null(cred))
})

test_that("authenticate_git fails for unknown protocol", {
  expect_error(authenticate_git(remote = "xyz:user/repo.git",
                                remote_avail = remote_avail),
               "The URL to the remote repository is using an unknown protocol")
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
  # Test print method
  expect_true("  $ git push origin master" %in% utils::capture.output(result))
})

test_that("wflow_git_pull can run in dry-run mode", {
  expect_silent(result <- wflow_git_pull(dry_run = TRUE, project = site_dir))
  expect_identical(result$remote, "origin")
  expect_identical(result$branch, "master")
  expect_identical(result$dry_run, TRUE)
  # Test print method
  expect_true("  $ git pull origin master" %in% utils::capture.output(result))
})
