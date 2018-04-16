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

# Test wflow_git_commit ------------------------------------------------------------

test_that("wflow_git_commit can commit one new file", {
  f1 <- file.path(site_dir, "f1.txt")
  file.create(f1)
  expect_silent(actual <- wflow_git_commit(f1, project = site_dir))
  expect_true(f1 %in% actual$commit_files)
  recent <- commits(r, n = 1)[[1]]
  expect_identical(actual$commit@sha, recent@sha)
  actual_print <- paste(utils::capture.output(actual), collapse = "\n")
  expect_true(grepl(sprintf("\\$ git add %s", f1), actual_print))
})

test_that("wflow_git_commit can commit multiple new files", {
  f2 <- file.path(site_dir, "f2.txt")
  f3 <- file.path(site_dir, "f3.txt")
  file.create(f2, f3)
  expect_silent(actual <- wflow_git_commit(c(f2, f3), project = site_dir))
  expect_identical(actual$commit_files, c(f2, f3))
  recent <- commits(r, n = 1)[[1]]
  expect_identical(actual$commit@sha, recent@sha)
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
  file.create(untracked)
  on.exit(file.remove(untracked))
  for (f in tracked)
    cat("edit", file = f, append = TRUE)
  expect_silent(actual <- wflow_git_commit(all = TRUE, project = site_dir))
  expect_identical(actual$commit_files, tracked)
  recent <- commits(r, n = 1)[[1]]
  expect_identical(actual$commit@sha, recent@sha)
})

test_that("wflow_git_commit does not affect Git repo if `dry_run = TRUE`", {
  before <- commits(r, n = 1)[[1]]
  tmp_file <- file.path(site_dir, "tmp.txt")
  file.create(tmp_file)
  on.exit(file.remove(tmp_file))
  expect_silent(wflow_git_commit(files = tmp_file, dry_run = TRUE,
                             project = site_dir))
  after <- commits(r, n = 1)[[1]]
  expect_identical(after, before)
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
