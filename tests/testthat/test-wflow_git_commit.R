context("wflow_git_commit")

# Setup -----------------------------------------------------------------------

library("git2r")
# start project in a tempdir
site_dir <- tempfile("test-wflow_git_commit-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE, user.name = "Test Name",
                             user.email = "test@email"))
site_dir <- workflowr:::relative(site_dir)
on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))
r <- repository(path = site_dir)
s <- wflow_status(project = site_dir)

# Load helper function local_no_gitconfig()
source("helpers.R", local = TRUE)

# Test wflow_git_commit --------------------------------------------------------

test_that("wflow_git_commit can commit one new file", {
  f1 <- file.path(site_dir, "f1.txt")
  fs::file_create(f1)
  expect_silent(actual <- wflow_git_commit(f1, project = site_dir))
  expect_true(f1 %in% actual$commit_files)
  recent <- commits(r, n = 1)[[1]]
  expect_identical(git2r_slot(actual$commit, "sha"), git2r_slot(recent, "sha"))
  actual_print <- paste(utils::capture.output(actual), collapse = "\n")
  expect_true(grepl(sprintf("\\$ git add %s", f1), actual_print))
})

test_that("wflow_git_commit can commit multiple new files", {
  f2 <- file.path(site_dir, "f2.txt")
  f3 <- file.path(site_dir, "f3.txt")
  fs::file_create(c(f2, f3))
  expect_silent(actual <- wflow_git_commit(c(f2, f3), project = site_dir))
  expect_identical(actual$commit_files, c(f2, f3))
  recent <- commits(r, n = 1)[[1]]
  expect_identical(git2r_slot(actual$commit, "sha"), git2r_slot(recent, "sha"))
  actual_print <- paste(utils::capture.output(actual), collapse = "\n")
  expect_true(grepl(sprintf("\\$ git add %s %s", f2, f3), actual_print))
})

test_that("wflow_git_commit creates a commit message", {
  o <- wflow_git_commit(all = TRUE, dry_run = TRUE, project = site_dir)
  expect_identical(o$message,
                   "wflow_git_commit(all = TRUE, dry_run = TRUE, project = site_dir)")
  o_print <- paste(utils::capture.output(o), collapse = "\n")
  # Need to use `fixed = TRUE` b/c of the parentheses in the message
  expect_true(grepl(sprintf("$ git commit -a -m \"%s\"", o$message), o_print,
                    fixed = TRUE))

  o <- wflow_git_commit(message = c("a", "b", "c"),
                    all = TRUE, dry_run = TRUE, project = site_dir)
  expect_identical(o$message, "a b c")
  o_print <- paste(utils::capture.output(o), collapse = "\n")
  expect_true(grepl(sprintf("\\$ git commit -a -m \"%s\"", o$message), o_print))

  o <- wflow_git_commit(message = "Example commit message",
                    all = TRUE, dry_run = TRUE, project = site_dir)
  expect_identical(o$message, "Example commit message")
  o_print <- paste(utils::capture.output(o), collapse = "\n")
  expect_true(grepl(sprintf("\\$ git commit -a -m \"%s\"", o$message), o_print))
})

test_that("wflow_git_commit can commit all tracked files", {
  tracked <- file.path(site_dir, "analysis",
                       c("about.Rmd", "index.Rmd", "license.Rmd"))
  # Create a temporary untracked file that should not be committed
  untracked <- file.path(site_dir, "analysis", "untracked.Rmd")
  fs::file_create(untracked)
  on.exit(fs::file_delete(untracked))
  for (f in tracked)
    cat("edit\n", file = f, append = TRUE)
  expect_silent(actual <- wflow_git_commit(all = TRUE, project = site_dir))
  expect_identical(actual$commit_files, tracked)
  recent <- commits(r, n = 1)[[1]]
  expect_identical(git2r_slot(actual$commit, "sha"), git2r_slot(recent, "sha"))
})

test_that("wflow_git_commit does not affect Git repo if `dry_run = TRUE`", {
  before <- commits(r, n = 1)[[1]]
  tmp_file <- file.path(site_dir, "tmp.txt")
  fs::file_create(tmp_file)
  on.exit(fs::file_delete(tmp_file))
  expect_silent(wflow_git_commit(files = tmp_file, dry_run = TRUE,
                             project = site_dir))
  after <- commits(r, n = 1)[[1]]
  expect_identical(after, before)
})

test_that("wflow_git_commit can perform the initial commit", {
  cwd <- getwd()
  on.exit(setwd(cwd))
  x <- tempfile()
  on.exit(unlink(x, recursive = TRUE), add = TRUE)

  o_start <- wflow_start(x, git = FALSE)
  expect_null(o_start$commit)

  r <- init(x)
  config(r, user.name = "Test Name", user.email = "test@email")
  o <- wflow_git_commit(c("*", ".gitignore", ".Rprofile"),
                        message = "Initial commit", project = x)
  expect_equal(length(commits(r)), 1)
  s <- status(r)
  expect_equal(length(s$untracked) + length(s$unstaged) + length(s$staged), 0)
})

# Test wflow_git_commit_ -------------------------------------------------------

test_that("wflow_git_commit_ can commit deleted files", {
  commit_current <- commits(r, n = 1)[[1]]
  on.exit(reset(commit_current, reset_type = "hard"))

  index <- file.path(s$analysis, "index.Rmd")
  about <- file.path(s$analysis, "about.Rmd")
  cat("edit\n", file = index, append = TRUE)
  fs::file_delete(about)
  observed <- workflowr:::wflow_git_commit_(c(index, about),
                                            message = "Edit and delete",
                                            project = site_dir)
  expect_true(index %in% observed$commit_files)
  expect_true(about %in% observed$commit_files)
})

test_that("wflow_git_commit_ can commit deleted files from project root", {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(s$root)

  commit_current <- commits(r, n = 1)[[1]]
  on.exit(reset(commit_current, reset_type = "hard"), add = TRUE)

  index <- "analysis/index.Rmd"
  about <- "analysis/about.Rmd"
  cat("edit\n", file = index, append = TRUE)
  fs::file_delete(about)
  observed <- workflowr:::wflow_git_commit_(c(index, about),
                                            message = "Edit and delete")
  expect_true(index %in% observed$commit_files)
  expect_true(about %in% observed$commit_files)
})

test_that("wflow_git_commit_ can commit deleted files from project subdir", {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(s$analysis)

  commit_current <- commits(r, n = 1)[[1]]
  on.exit(reset(commit_current, reset_type = "hard"), add = TRUE)

  index <- "index.Rmd"
  about <- "about.Rmd"
  cat("edit\n", file = index, append = TRUE)
  fs::file_delete(about)
  observed <- workflowr:::wflow_git_commit_(c(index, about),
                                            message = "Edit and delete")
  expect_true(index %in% observed$commit_files)
  expect_true(about %in% observed$commit_files)
})

# Test error handling ----------------------------------------------------------

test_that("wflow_git_commit fails with invalid argument for files", {
  expect_error(wflow_git_commit(files = 1, project = site_dir),
               "files must be NULL or a character vector of filenames")
  expect_error(wflow_git_commit(files = "nonexistent.Rmd", project = site_dir),
               "Not all files exist. Check the paths to the files")
})

test_that("wflow_git_commit fails if no files and `all = FALSE`", {
  expect_error(wflow_git_commit(files = NULL, all = FALSE, project = site_dir),
               "Must specify files to commit, set `all = TRUE`, or both")
  # And that should be the default
  expect_error(wflow_git_commit(project = site_dir),
               "Must specify files to commit, set `all = TRUE`, or both")
})

test_that("wflow_git_commit provides interpretable error message if commit fails", {
  expect_error(wflow_git_commit(files = file.path(site_dir, "analysis", "about.Rmd"),
                            project = site_dir),
               "Commit failed because no files were added.")
})

test_that("wflow_git_commit fails early if no Git repository", {
  git_orig <- file.path(site_dir, ".git")
  git_tmp <- file.path(site_dir, "nothing-to-see-here")
  on.exit(file.rename(git_tmp, git_orig))
  file.rename(git_orig, git_tmp)

  expect_error(wflow_git_commit(all = TRUE, project = site_dir),
               "No Git repository detected.")
  expect_error(wflow_git_commit(Sys.glob(file.path(site_dir, "analysis", "*Rmd")),
                                project = site_dir),
               "No Git repository detected.")
})

test_that("wflow_git_commit throws an error if user.name and user.email are not set", {

  skip_on_cran()

  # local_no_gitconfig() is defined in tests/testthat/helpers.R
  local_no_gitconfig("-workflowr")

  # Also have to remove local ./.git/config in the project's Git repo. Couldn't
  # figure out a good way to do this with withr. Couldn't get to "restore"
  # function to run at the end of the function call.
  gitconfig <- file.path(site_dir, ".git", "config")
  gitconfig_tmp <- file.path(tempdir(), "config")
  file.rename(gitconfig, gitconfig_tmp)
  on.exit(file.rename(gitconfig_tmp, gitconfig), add = TRUE)

  expect_error(wflow_git_commit(project = site_dir),
               "You must set your user.name and user.email for Git first")
  expect_error(wflow_git_commit(project = site_dir),
               "wflow_git_commit")
})

test_that("wflow_git_commit fails early if merge conflicts detected", {
  x <- tempfile("test-merge-conflicts-")
  suppressMessages(wflow_start(x, change_wd = FALSE, user.name = "Test Name",
                               user.email = "test@email"))
  x <- workflowr:::relative(x)
  on.exit(unlink(x, recursive = TRUE, force = TRUE))
  r <- repository(path = x)
  s <- wflow_status(project = x)

  # Edit index.Rmd on new branch
  rmd <- file.path(s$analysis, "index.Rmd")
  checkout(r, "b2", create = TRUE)
  cat("\nedit on b2\n", file = rmd, append = TRUE)
  add(r, rmd)
  commit(r, "edit index.Rmd on b2")
  # Edit index.Rmd on master branch
  checkout(r, "master")
  cat("\nedit on master\n", file = rmd, append = TRUE)
  add(r, rmd)
  commit(r, "edit index.Rmd on master")
  # Generate merge conflict
  workflowr:::git2r_merge(r, "b2")

  skip_on_cran()

  # Attempt to publish
  expect_error(wflow_publish(rmd, project = x), rmd)
})
