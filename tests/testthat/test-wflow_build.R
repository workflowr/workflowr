context("wflow_build")

# Setup ------------------------------------------------------------------------

# start project in a tempdir
site_dir <- tempfile("test-build-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE))
on.exit(unlink(site_dir, recursive = TRUE))
s <- wflow_status(project = site_dir)
# cwd <- getwd()
# on.exit(setwd(cwd), add = TRUE)
# setwd(s$analysis)

rmd <- rownames(s$status)
stopifnot(length(rmd) > 0)
# Expected html files
html <- to_html(rmd, outdir = s$docs)

# Test wflow_build -------------------------------------------------------------

test_that("wflow_build builds the specified files", {
  # Dry run for file 1
  expect_silent(actual <- wflow_build(rmd[1], dry_run = TRUE,
                                      project = site_dir))
  expect_identical(actual$built, rmd[1])
  expect_false(file.exists(html[1]))
  # Build file 1
  expect_message(actual <- wflow_build(rmd[1], dry_run = FALSE,
                                       project = site_dir),
                 rmd[1])
  expect_identical(actual$built, rmd[1])
  expect_true(file.exists(html[1]))
  # Dry run for files 2 & 3
  expect_silent(actual <- wflow_build(rmd[2:3], dry_run = TRUE,
                                      project = site_dir))
  expect_identical(actual$built, rmd[2:3])
  expect_false(any(file.exists(html[2:3])))
  # Build files 2 & 3
  expect_message(actual <- wflow_build(rmd[2:3], dry_run = FALSE,
                                       project = site_dir),
                 rmd[2])
  expect_identical(actual$built, rmd[2:3])
  expect_true(all(file.exists(html[2:3])))
})

test_that("wflow_build can run in 'make' mode", {
  # Reset modifications of rmd files
  file.create(rmd)
  expect_silent(actual <- wflow_build(dry_run = TRUE, project = site_dir))
  expect_identical(actual$built, rmd)
  expect_true(actual$make)
  expect_message(actual <- wflow_build(project = site_dir), rmd[1])
  expect_identical(actual$built, rmd)
  # No file should be built now
  expect_silent(actual <- wflow_build(project = site_dir))
  expect_identical(actual$built, character(0))
  # Reset modification of file 1 only
  file.create(rmd[1])
  expect_message(actual <- wflow_build(project = site_dir), rmd[1])
  expect_identical(actual$built, rmd[1])
})

# Publish the files
suppressMessages(wflow_publish(files = rmd, project = site_dir))

test_that("wflow_build update builds published files with modifications", {
  cat("edit", file = rmd[1], append = TRUE)
  wflow_commit(rmd[1], project = site_dir)
  expect_silent(actual <- wflow_build(update = TRUE, dry_run = TRUE,
                                      project = site_dir))
  expect_identical(actual$built, rmd[1])
  expect_true(actual$update)
  expect_message(actual <- wflow_build(update = TRUE, project = site_dir),
                 rmd[1])
  expect_identical(actual$built, rmd[1])
})

test_that("wflow_build update builds published files with modifications", {
  cat("edit", file = rmd[1], append = TRUE)
  wflow_build(project = site_dir)
  wflow_commit(rmd[1], project = site_dir)
  expect_silent(actual <- wflow_build(update = TRUE, dry_run = TRUE,
                                      project = site_dir))
  expect_identical(actual$built, rmd[1])
  expect_true(actual$update)
  expect_message(actual <- wflow_build(update = TRUE, project = site_dir),
                 rmd[1])
  expect_identical(actual$built, rmd[1])
})

test_that("wflow_build republish builds all published files", {
  wflow_build(project = site_dir)
  html_mtime_pre <- file.mtime(html)
  expect_message(actual <- wflow_build(republish = TRUE, project = site_dir),
                 rmd[1])
  expect_true(actual$republish)
  expect_identical(actual$built, rmd)
  html_mtime_post <- file.mtime(html)
  expect_true(all(html_mtime_post > html_mtime_pre))
})
