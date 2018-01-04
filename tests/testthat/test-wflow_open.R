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
site_dir <- base::tempfile("test-wflow_open-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE))
site_dir <- workflowr:::relative(site_dir)
if (!interactive()) on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))

# Test wflow_open --------------------------------------------------------------

test_that("wflow_open creates a new file, but does not overwrite", {

  if (skipping)
    skip("Must be run manually.")

  rmd <- wflow_open("test.Rmd", change_wd = FALSE, open_file = FALSE,
                    project = site_dir)
  expect_true(file.exists(rmd))
  modification_time_pre <- file.mtime(rmd)
  Sys.sleep(2)
  rmd2 <- wflow_open("test.Rmd", change_wd = FALSE, open_file = FALSE,
                     project = site_dir)
  expect_identical(rmd2, rmd)
  modification_time_post <- file.mtime(rmd2)
  expect_identical(modification_time_post, modification_time_pre)
})

test_that("wflow_open changes the working directory", {

  if (skipping)
    skip("Must be run manually.")

  wd_pre <- getwd()
  on.exit(setwd(wd_pre))
  expect_silent(rmd <- wflow_open("do-not-change-wd.Rmd", change_wd = FALSE,
                           open_file = FALSE, project = site_dir))
  expect_true(file.exists(rmd))
  wd_same <- getwd()
  expect_identical(wd_pre, wd_same)
  expect_message(rmd <- wflow_open("do-change-wd.Rmd", change_wd = TRUE,
                            open_file = FALSE, project = site_dir),
                 "Working directory was changed.")
  expect_true(file.exists(rmd))
  wd_change <- workflowr:::relative(getwd(), start = wd_pre)
  expect_identical(wd_change, file.path(site_dir, "analysis"))
  expect_silent(rmd <- wflow_open("no-need-to-change-wd.Rmd",
                    change_wd = TRUE, open_file = FALSE,
                    project = site_dir))
  expect_true(file.exists(rmd))
})

test_that("wflow_open can accept multiple files", {

  if (skipping)
    skip("Must be run manually.")

  rmd_multi <- paste0(1:3, ".Rmd")
  rmd <- wflow_open(rmd_multi, change_wd = FALSE, open_file = FALSE,
                               project = site_dir)
  expect_true(all(file.exists(rmd)))
  modification_time_pre <- file.mtime(rmd)
  Sys.sleep(2)
  rmd2 <- wflow_open(rmd_multi, change_wd = FALSE, open_file = FALSE,
                                 project = site_dir)
  expect_identical(rmd2, rmd)
  modification_time_post <- file.mtime(rmd)
  expect_identical(modification_time_post, modification_time_pre)
})

test_that("wflow_open can accept basename, full paths, and wrong paths", {

  if (skipping)
    skip("Must be run manually.")

  rmd_paths <- c("basename.Rmd",
                 file.path(site_dir, "analysis", "full.Rmd"),
                 file.path(site_dir, "code", "wrong.Rmd"))
  rmd <- wflow_open(rmd_paths, change_wd = FALSE, open_file = FALSE,
                    project = site_dir)
  expect_true(all(file.exists(rmd)))
  modification_time_pre <- file.mtime(rmd)
  Sys.sleep(2)
  rmd2 <- wflow_open(rmd_paths, change_wd = FALSE, open_file = FALSE,
                                 project = site_dir)
  expect_identical(rmd2, rmd)
  modification_time_post <- file.mtime(rmd)
  expect_identical(modification_time_post, modification_time_pre)
})

test_that("wflow_open can save outside of analysis/ when project = NULL", {

  if (skipping)
    skip("Must be run manually.")

  # When project = NULL, wflow_open will create output directories if needed, and
  # switches the working directory to the path of the first input file.
  cwd <- getwd()
  on.exit(setwd(cwd))
  location_exist <- base::tempfile("test-wflow_open-exist-")
  dir.create(location_exist)
  location_exist <- workflowr:::relative(location_exist)
  on.exit(unlink(location_exist, recursive = TRUE, force = TRUE), add = TRUE)
  testfile1 <- file.path(location_exist, "exist.Rmd")
  location_nonexist <- base::tempfile("test-wflow_open-nonexist-")
  on.exit(unlink(location_nonexist, recursive = TRUE, force = TRUE), add = TRUE)
  testfile2 <- file.path(location_nonexist, "nonexist.Rmd")

  outfile <- wflow_open(c(testfile1, testfile2), change_wd = TRUE,
                        open_file = FALSE, project = NULL)
  expect_true(all(file.exists(outfile)))
  expect_identical(outfile, workflowr:::relative(c(testfile1, testfile2)))
})

test_that("wflow_open can create a standalone version of the template", {

  if (skipping)
    skip("Must be run manually.")

  file_standalone <- wflow_open("standalone.Rmd", change_wd = FALSE,
                                open_file = FALSE, standalone = TRUE,
                                project = site_dir)
  lines_standalone <- readLines(file_standalone)
  expect_true("sessionInfo()" %in% lines_standalone)

  file_default <- wflow_open("default.Rmd", change_wd = FALSE,
                             open_file = FALSE, standalone = FALSE,
                             project = site_dir)
  lines_default <- readLines(file_default)
  expect_false("sessionInfo()" %in% lines_default)
})

# Errors -----------------------------------------------------------------------

test_that("wflow_open rejects filenames without Rmd or rmd extension", {

  if (skipping)
    skip("Must be run manually.")

  expect_error(wflow_open("invalid-ext.md", change_wd = FALSE, open_file = FALSE,
                          project = site_dir),
                 "R Markdown files must have the extension Rmd or rmd.")
  expect_error(wflow_open("no-ext", change_wd = FALSE, open_file = FALSE,
                            project = site_dir),
                 "R Markdown files must have the extension Rmd or rmd.")
  expect_silent(wflow_open("valid-ext.Rmd", change_wd = FALSE, open_file = FALSE,
                           project = site_dir))
  expect_silent(wflow_open("valid-ext.rmd", change_wd = FALSE, open_file = FALSE,
                           project = site_dir))
})
