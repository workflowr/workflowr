context("wflow_status")

# Setup ------------------------------------------------------------------------

# Setup workflowr project for testing
site_dir <- tempfile("test-wflow_status-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE))
# Cleanup
on.exit(unlink(site_dir, recursive = TRUE))
# Build site
suppressMessages(utils::capture.output(wflow_build(path = site_dir)))

# Test wflow_status --------------------------------------------------------------

s <- wflow_status(project = site_dir)

wd <- getwd()

test_that("wflow_status identifies root directory.", {
  expected <- relpath(site_dir, start = wd)
  actual <- s$root
  expect_identical(actual, expected)
})

test_that("wflow_status identifies analysis directory.", {
  expected <- file.path(site_dir, "analysis")
  expected <- relpath(expected, start = wd)
  actual <- s$analysis
  expect_identical(actual, expected)
})

test_that("wflow_status identifies docs directory.", {
  expected <- file.path(site_dir, "docs")
  expected <- relpath(expected, start = wd)
  actual <- s$docs
  expect_identical(actual, expected)
})

test_that("wflow_status identifies Git directory.", {
  expected <- file.path(site_dir, ".git")
  expected <- relpath(expected, start = wd)
  actual <- s$git
  expect_identical(actual, expected)
})

# Warnings and Errors ----------------------------------------------------------

test_that("wflow_status throws error if no Git repository.", {
  git_original <- file.path(site_dir, ".git")
  git_replace <-  file.path(site_dir, ".git2")
  on.exit(file.rename(git_replace, git_original))
  file.rename(git_original, git_replace)
  expect_error(s <- wflow_status(project = site_dir),
               "A Git repository is required for this functionality.")
})
