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

test_that("wflow_status identifies working directory.", {
  expected <- getwd()
  actual <- s$wd
  expect_identical(actual, expected)
})

test_that("wflow_status identifies root directory.", {
  expected <- site_dir
  actual <- s$root
  expect_identical(actual, expected)
})

test_that("wflow_status identifies analysis directory.", {
  expected <- file.path(site_dir, "analysis")
  actual <- s$analysis
  expect_identical(actual, expected)
})

test_that("wflow_status identifies docs directory.", {
  expected <- file.path(site_dir, "docs")
  actual <- s$docs
  expect_identical(actual, expected)
})

test_that("wflow_status identifies Git directory.", {
  expected <- file.path(site_dir, ".git")
  actual <- s$git
  expect_identical(actual, expected)
})
