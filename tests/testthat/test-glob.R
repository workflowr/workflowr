context("glob")

# Setup ------------------------------------------------------------------------

# start project in a tempdir
site_dir <- tempfile("test-glob-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE))
on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))
site_dir <- workflowr:::absolute(site_dir)
s <- wflow_status(project = site_dir)

rmd <- rownames(s$status)
stopifnot(length(rmd) > 0)
# Expected html files
html <- workflowr:::to_html(rmd, outdir = s$docs)

# Test detect_glob -------------------------------------------------------------

test_that("detect_glob can detect asterisks", {
  paths <- c("/a/b/c/*", "*/a/b/c/", "*/a/b/c/*", "/a/b/c/")
  expected <- c(TRUE, TRUE, TRUE, FALSE)
  actual <- workflowr:::detect_glob(paths)
  expect_identical(actual, expected)
})

test_that("detect_glob can detect question marks", {
  paths <- c("/a/b/c/?", "?/a/b/c/", "?/a/b/c/?", "/a/b/c/")
  expected <- c(TRUE, TRUE, TRUE, FALSE)
  actual <- workflowr:::detect_glob(paths)
  expect_identical(actual, expected)
})

test_that("detect_glob can detect square brackets", {
  paths <- c("/a/b/c/[def]", "[def]/a/b/c/", "[def]/a/b/c/[def]", "/a/b/c/")
  expected <- c(TRUE, TRUE, TRUE, FALSE)
  actual <- workflowr:::detect_glob(paths)
  expect_identical(actual, expected)
})

test_that("detect_glob can detect square brackets", {
  paths <- c("/a/b/c/[def]", "[def]/a/b/c/", "[def]/a/b/c/[def]", "/a/b/c/")
  expected <- c(TRUE, TRUE, TRUE, FALSE)
  actual <- workflowr:::detect_glob(paths)
  expect_identical(actual, expected)
})

test_that("detect_glob works", {
  paths <- c("*/a/b?/c/[def]", "/a/b*/c/", "/a/b?/c/", "/a/b[]/c/",
             "/a/b/c/", "12345", "~:", tempfile())
  expected <- c(TRUE, TRUE, TRUE, FALSE,
                FALSE, FALSE, FALSE, FALSE)
  actual <- workflowr:::detect_glob(paths)
  expect_identical(actual, expected)
})

# Test glob --------------------------------------------------------------------

test_that("glob does nothing if no globbing detected", {
  # Existing file
  x <- file.path(s$root, "README.md")
  expect_identical(workflowr:::glob(x), x)
  # Existing directory
  x <- file.path(s$root)
  expect_identical(workflowr:::glob(x), x)
  # Non-existing path
  x <- tempfile()
  expect_identical(workflowr:::glob(x), x)
})

test_that("glob obtains the same results as Sys.glob for file globs", {
  glob_str <- file.path(s$analysis, "*Rmd")
  expected <- Sys.glob(glob_str)
  actual <- workflowr:::glob(glob_str)
  expect_identical(actual, expected)
  expect_true(length(actual) > 0)
})

test_that("glob can process paths with and without globs", {
  glob_str <- file.path(s$analysis, "*Rmd")
  paths <- c(s$root, "a", glob_str, file.path(s$root, "README.md"))
  expected <- c(s$root, "a", Sys.glob(glob_str), file.path(s$root, "README.md"))
  actual <- workflowr:::glob(paths)
  expect_identical(actual, expected)
})

test_that("glob does not return duplicates", {
  glob_str <- file.path(s$analysis, "*Rmd")
  expected <- Sys.glob(glob_str)
  actual <- workflowr:::glob(c(glob_str, glob_str))
  expect_identical(actual, expected)
})

test_that("glob throws error for invalid glob", {
  valid <- file.path(s$analysis, "*Rmd")
  invalid <- file.path(s$analysis, "*xyz")
  expect_error(workflowr:::glob(c(valid, invalid)),
               sprintf("Invalid file glob:"))
  expect_error(workflowr:::glob(c(invalid, valid)),
               sprintf("Invalid file glob:"))
})

# Test file globbing -----------------------------------------------------------

test_that("wflow_build accepts file globs", {
  rmd_glob <- file.path(s$analysis, "*Rmd")
  build_w_glob <- wflow_build(rmd_glob, project = site_dir)
  expect_identical(Sys.glob(rmd_glob), build_w_glob$built)
  expect_error(wflow_build(file.path(s$analysis, "bad*blob.Rmd"), project = site_dir),
               "Invalid file glob:")
})
