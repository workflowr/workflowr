context("commit-external-git")

# Test wflow_commit used in conjuction with external Git commands

library("git2r")

# start project in a tempdir
project_name <- "test-commit-external-git"
site_dir <- tempfile(paste0(project_name, "-"))
suppressMessages(wflow_start(project_name, site_dir))
r <- repository(path = site_dir)

test_that("wflow_commit detects R Markdown files in root commit", {
  expect_identical(wflow_build(dry_run = TRUE, path = site_dir),
                   wflow_commit(dry_run = TRUE, path = site_dir))
})

# Commit site
suppressMessages(capture.output(wflow_commit(path = site_dir)))

test_that("wflow_commit creates HTML files", {
  html_files <- file.path(site_dir, "docs", c("about.html",
                                              "index.html",
                                              "license.html"))
  expect_true(all(file.exists(html_files)))
})

log <- commits(r)

test_that("wflow_commit creates a new commit", {
  expect_identical(log[[1]]@message, "Build site.")
})

test_that("wflow_commit include .nojekyll in commit", {
  files_in_commit <- workflowr:::obtain_files_in_commit(r, log[[1]])
  expect_true("docs/.nojekyll" %in% files_in_commit)
})

# Add a new R Markdown file
# Can't use wflow_open b/c of the issue with devtools shims
# new_rmd <- wflow_open("test.Rmd", change_wd = FALSE, open_file = FALSE,
#                     path = site_dir)
new_rmd <- file.path(site_dir, "analysis", "test.Rmd")
file.create(new_rmd)
new_html <- file.path(site_dir, "docs", "test.html")

test_that("wflow_build, but not wflow_commit, renders untracked Rmd file", {
  wflow_build_files <- wflow_build(dry_run = TRUE, path = site_dir)
  wflow_commit_files <- wflow_commit(dry_run = TRUE, path = site_dir)
  expect_true(new_rmd %in% wflow_build_files)
  expect_false(new_rmd %in% wflow_commit_files)
})

# Stage file
add(r, new_rmd)

test_that("wflow_build, but not wflow_commit, renders staged Rmd file", {
  wflow_build_files <- wflow_build(dry_run = TRUE, path = site_dir)
  expect_true(new_rmd %in% wflow_build_files)
  # It gives a warning if there are staged files
  expect_warning(wflow_commit(dry_run = TRUE, include_staged = TRUE,
                              path = site_dir),
                 "Files have already been added to the staging area.")
  # It fails by default if there are staged file
  expect_error(suppressWarnings(wflow_commit(dry_run = TRUE, path = site_dir)),
               "wflow_commit stopped because of files in the staging area.")
})

# Commit file
commit(r, new_rmd)

test_that("wflow_build **and** wflow_commit render committed Rmd file", {
  wflow_build_files <- wflow_build(dry_run = TRUE, path = site_dir)
  wflow_commit_files <- wflow_commit(dry_run = TRUE, path = site_dir)
  expect_true(new_rmd %in% wflow_build_files)
  expect_true(new_rmd %in% wflow_commit_files)
})

# Commit site
suppressMessages(capture.output(wflow_commit(path = site_dir)))

log <- commits(r)

test_that("wflow_commit includes latest html in commit, but not previous html", {
  files_in_commit <- workflowr:::obtain_files_in_commit(r, log[[1]])
  expect_true(file.path("docs", basename(new_html)) %in% files_in_commit)
  # Extract only files/directories in base of docs/
  files_in_docs <- sapply(strsplit(files_in_commit, split = "/"),
                          function(x) x[2])
  expect_true(sum(grepl("html$", files_in_docs)) == 1)
})

unlink(site_dir, recursive = TRUE)
