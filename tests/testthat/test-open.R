# These tests cannot be run automatically with `devtools::test`. Instead of
# locating the workflowr package installed in the local library, it locates the
# source code being tested. This breaks it, because the directory
# structure is different (the inst/ is removed when the package is installed). I
# suspect this has to do with devtools having its own `system.file` to override
# `base::system.file` (see the link to pkgload package below). The function
# `rmarkdown::draft` calls `system.file`, so there is no clear way to prevent
# this behavior. This is really frustrating. However, it can be tested manually
# using the following steps.

# https://github.com/r-pkgs/pkgload/blob/master/R/shims.r#L8

# I commented on an open issue on devtools about system.file causing problems
# with testthat.
# https://github.com/hadley/devtools/issues/1404#issuecomment-268361994

# 1. Restart R session (even if you haven't loaded devtools, running one of its
#    exported functions, e.g. `devtools::test`, pollutes the search path with
#    devtools_shims)
# 2. Source this file, test-open.R

if ("devtools_shims" %in% search()) {
  skipping <- TRUE
} else {
  skipping <- FALSE
  message("Running tests in test-open.R")
  library("testthat")
  library("workflowr")
}

context("wflow_open")

# Setup ------------------------------------------------------------------------

# start project in a tempdir
site_dir <- tempfile("test-wflow_open-")
suppressMessages(wflow_start("Test wflow_open", site_dir))
if (!interactive()) on.exit(unlink(site_dir, recursive = TRUE))

# Test wflow_open --------------------------------------------------------------

test_that("wflow_open creates a new file, but does not overwrite", {

  if (skipping)
    skip("Must be run manually.")

  full_file_path <- wflow_open("test.Rmd", change_wd = FALSE, open_file = FALSE,
                             path = site_dir)
  expect_true(file.exists(full_file_path))
  modification_time_pre <- file.mtime(full_file_path)
  full_file_path_2 <- wflow_open("test.Rmd", change_wd = FALSE, open_file = FALSE,
                               path = site_dir)
  expect_identical(full_file_path_2, full_file_path)
  modification_time_post <- file.mtime(full_file_path)
  expect_identical(modification_time_post, modification_time_pre)
})

test_that("wflow_open changes the working directory", {

  if (skipping)
    skip("Must be run manually.")

  wd_pre <- getwd()
  wflow_open("do-not-change-wd.Rmd", change_wd = FALSE, open_file = FALSE,
           path = site_dir)
  wd_same <- getwd()
  expect_identical(wd_pre, wd_same)
  wflow_open("do-change-wd.Rmd", change_wd = TRUE, open_file = FALSE,
           path = site_dir)
  wd_change <- getwd()
  expect_identical(wd_change, file.path(site_dir, "analysis"))
  # Cleanup
  setwd(wd_pre)
})

test_that("wflow_open can accept multiple files", {

  if (skipping)
    skip("Must be run manually.")
  rmd_multi <- paste0(1:3, ".Rmd")
  full_file_path <- wflow_open(rmd_multi, change_wd = FALSE, open_file = FALSE,
                               path = site_dir)
  expect_true(all(file.exists(full_file_path)))
  modification_time_pre <- file.mtime(full_file_path)
  full_file_path_2 <- wflow_open(rmd_multi, change_wd = FALSE, open_file = FALSE,
                                 path = site_dir)
  expect_identical(full_file_path_2, full_file_path)
  modification_time_post <- file.mtime(full_file_path)
  expect_identical(modification_time_post, modification_time_pre)
})

test_that("wflow_open can accept basename, full paths, and wrong paths", {

  if (skipping)
    skip("Must be run manually.")
  rmd_paths <- c("basename.Rmd",
                 file.path(site_dir, "analysis", "full.Rmd"),
                 file.path(site_dir, "code", "wrong.Rmd"))
  expect_warning(full_file_path <-
                   wflow_open(rmd_paths, change_wd = FALSE, open_file = FALSE,
                               path = site_dir),
                 "Input file had invalid subdirectory specified.")
  expect_true(all(file.exists(full_file_path)))
  modification_time_pre <- file.mtime(full_file_path)
  full_file_path_2 <- wflow_open(rmd_paths, change_wd = FALSE, open_file = FALSE,
                                 path = site_dir)
  expect_identical(full_file_path_2, full_file_path)
  modification_time_post <- file.mtime(full_file_path)
  expect_identical(modification_time_post, modification_time_pre)
})

# Errors -----------------------------------------------------------------------

test_that("wflow_open rejects filenames without Rmd or rmd extension", {

  if (skipping)
    skip("Must be run manually.")

  expect_error(wflow_open("invalid-ext.md", change_wd = FALSE, open_file = FALSE,
                          path = site_dir),
                 "R Markdown files must have the extension Rmd or rmd.")
  expect_error(wflow_open("no-ext", change_wd = FALSE, open_file = FALSE,
                            path = site_dir),
                 "R Markdown files must have the extension Rmd or rmd.")
  expect_silent(wflow_open("valid-ext.Rmd", change_wd = FALSE, open_file = FALSE,
                           path = site_dir))
  expect_silent(wflow_open("valid-ext.rmd", change_wd = FALSE, open_file = FALSE,
                           path = site_dir))
})
