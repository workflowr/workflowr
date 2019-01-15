context("glob")

# Setup ------------------------------------------------------------------------

# start project in a tempdir
site_dir <- tempfile("test-glob-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE, user.name = "Test Name",
                             user.email = "test@email"))
if (!interactive()) on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))
site_dir <- workflowr:::absolute(site_dir)
s <- wflow_status(project = site_dir)
rmd_glob <- file.path(s$analysis, "*Rmd")

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
  expected <- Sys.glob(rmd_glob)
  actual <- workflowr:::glob(rmd_glob)
  expect_identical(actual, expected)
  expect_true(length(actual) > 0)
})

test_that("glob can process paths with and without globs", {
  paths <- c(s$root, "a", rmd_glob, file.path(s$root, "README.md"))
  expected <- c(s$root, "a", Sys.glob(rmd_glob), file.path(s$root, "README.md"))
  actual <- workflowr:::glob(paths)
  expect_identical(actual, expected)
})

test_that("glob does not return duplicates", {
  expected <- Sys.glob(rmd_glob)
  actual <- workflowr:::glob(c(rmd_glob, rmd_glob))
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

expected <- Sys.glob(rmd_glob)

test_that("wflow_build accepts file globs", {

  skip_on_cran()

  actual <- wflow_build(rmd_glob, dry_run = TRUE, project = site_dir)
  expect_identical(actual$files, expected)
  expect_error(wflow_build(file.path(s$analysis, "bad*blob.Rmd"),
                            dry_run = TRUE, project = site_dir),
               "Invalid file glob:")
})

test_that("wflow_git_commit accepts file globs", {
  actual <- wflow_git_commit(rmd_glob, dry_run = TRUE, project = site_dir)
  expect_identical(actual$files, expected)
  expect_error(wflow_git_commit(file.path(s$analysis, "bad*blob.Rmd"),
                            dry_run = TRUE, project = site_dir),
               "Invalid file glob:")
})

test_that("wflow_open accepts file globs", {
  rmd_glob_expanded <- Sys.glob(rmd_glob)
  open_w_glob <- wflow_open(rmd_glob, change_wd = FALSE, edit_in_rstudio = FALSE,
                            project = site_dir)
  expect_identical(relative(open_w_glob$files), rmd_glob_expanded)

  expect_error(wflow_open(file.path(s$analysis, "bad*blob.Rmd"),
                          project = site_dir),
               "Invalid file glob:")

  rmd_new <- file.path(s$analysis, "new.Rmd")
  on.exit(fs::file_delete(rmd_new))
  open_w_glob_new <- wflow_open(c(rmd_glob, rmd_new), change_wd = FALSE,
                                edit_in_rstudio = FALSE, project = site_dir)
  expect_identical(relative(open_w_glob_new$files), c(rmd_glob_expanded, rmd_new))
  expect_true(fs::file_exists(rmd_new))
})

test_that("wflow_publish accepts file globs", {

  skip_on_cran()

  actual <- wflow_publish(rmd_glob, dry_run = TRUE, project = site_dir)
  expect_identical(actual$step2$files, expected)
  expect_error(wflow_publish(file.path(s$analysis, "bad*blob.Rmd"),
                             dry_run = TRUE, project = site_dir),
               "Invalid file glob:")
})

test_that("wflow_remove accepts file globs", {
  actual <- wflow_remove(rmd_glob, dry_run = TRUE, project = site_dir)
  expect_identical(actual$files, expected)
  expect_error(wflow_remove(file.path(s$analysis, "bad*blob.Rmd"),
                            dry_run = TRUE, project = site_dir),
               "Invalid file glob:")
})

test_that("wflow_status accepts file globs", {
  actual <- wflow_status(rmd_glob, project = site_dir)
  expect_identical(rownames(actual$status), expected)
  expect_error(wflow_status(file.path(s$analysis, "bad*blob.Rmd"),
                            project = site_dir),
               "Invalid file glob:")
})

test_that("wflow_view accepts file globs", {
  html <- workflowr:::to_html(expected, outdir = s$docs)
  fs::file_create(html)
  on.exit(fs::file_delete(html))
  actual <- wflow_view(rmd_glob, dry_run = TRUE, project = site_dir)
  expect_identical(actual$opened, html)
  expect_error(wflow_view(file.path(s$analysis, "bad*blob.Rmd"),
                            dry_run = TRUE, project = site_dir),
               "Invalid file glob:")
})
