context("commit-auto-git")

# Test wflow_commit when used to for all commit actions, i.e. both source and
# website files.

library("git2r")

# start project in a tempdir
project_name <- "test-commit-auto-git"
site_dir <- tempfile(paste0(project_name, "-"))
suppressMessages(wflow_start(project_name, site_dir))
r <- repository(path = site_dir)

# Commit the site so that original Rmd files are already built
suppressMessages(wflow_commit(path = site_dir))

# Test that commit_site can commit a file and then commit site

# Create some fake R Markdown files
# Unfortunately cannot use wflow_open here b/c of devtools
test_rmd <- file.path(site_dir, paste0("analysis/", 1:3, ".Rmd"))
for (i in 1:3) {
  file.copy("files/workflowr-template.Rmd", test_rmd[i])
}
# Expected html files
test_html <- stringr::str_replace(test_rmd, "Rmd$", "html")
test_html <- stringr::str_replace(test_html, "/analysis/", "/docs/")

test_that("commit_site can commit one file and then commit site", {
  expect_message(dry_run_files <- wflow_commit(commit_files = test_rmd[1],
                                               dry_run = TRUE, path = site_dir),
                 "You are planning to commit the following files before building the site:")
  expect_identical(dry_run_files, character(0))
  output <- capture_messages(built_files <- wflow_commit(
    commit_files = test_rmd[1],
    path = site_dir))
  expect_identical(test_rmd[1], built_files)
  expect_identical(basename(test_rmd[1]),
                   stringr::str_split(output[output != ""], " ",
                                      simplify = TRUE)[, 2] %>%
    stringr::str_replace_all(., "\n", ""))
  expect_true(all(file.exists(test_html[1])))
  log <- commits(r)
  expect_identical(log[[1]]@message, "Build site.")
  expect_identical(log[[2]]@message, "Files commited by wflow_commit.")
})

test_that("commit_site can commit multiple files and then commit site", {
  expect_message(dry_run_files <- wflow_commit(commit_files = test_rmd[2:3],
                                               dry_run = TRUE, path = site_dir),
                 "You are planning to commit the following files before building the site:")
  expect_identical(dry_run_files, character(0))
  output <- capture_messages(built_files <- wflow_commit(
    commit_files = test_rmd[2:3],
    path = site_dir))
  expect_identical(test_rmd[2:3], built_files)
  expect_identical(basename(test_rmd[2:3]),
                   stringr::str_split(output[output != ""], " ",
                                      simplify = TRUE)[, 2] %>%
                   stringr::str_replace_all(., "\n", ""))
  expect_true(all(file.exists(test_html[2:3])))
  log <- commits(r)
  expect_identical(log[[1]]@message, "Build site.")
  expect_identical(log[[2]]@message, "Files commited by wflow_commit.")
})

test_that("commit_site does not commit files that have not changed", {
  expect_message(dry_run_files <- wflow_commit(commit_files = test_rmd[2:3],
                                               dry_run = TRUE, path = site_dir),
                 "You are planning to commit the following files before building the site:")
  expect_identical(dry_run_files, character(0))
  last_commit_pre <- commits(r)[[1]]@sha
  expect_warning(output <- capture_messages(
    built_files <- wflow_commit(commit_files = test_rmd[2:3], path = site_dir)),
    "None of the commit_files provided were committed, presumably because they have not been updated.")
  expect_identical(character(0), built_files)
  expect_identical(output, "Everything up-to-date\n")
  last_commit_post <- commits(r)[[1]]@sha
  expect_identical(last_commit_pre, last_commit_post)
})

# Test `all = TRUE`
#
# Create a file and commit it. This will change the most recent commit SHA1.
# Then run wflow_commit(all = TRUE). All the HTML files should be re-built, and
# they should all be committed because they'll have a new SHA1 at the top.
new_file <- file.path(site_dir, "code", "script.R")
file.create(new_file)
all_rmd <- list.files(path = file.path(site_dir, "analysis"),
                      pattern = "^[^_].*Rmd$",
                      full.names = TRUE)
all_html <- stringr::str_replace(all_rmd, "Rmd$", "html")
all_html <- stringr::str_replace(all_html, "/analysis/", "/docs/")

test_that("commit_site can re-build all files and then commit site", {
  expect_message(dry_run_files <- wflow_commit(all = TRUE,
                                               commit_files = new_file,
                                               dry_run = TRUE, path = site_dir),
                 "You are planning to commit the following files before building the site:")
  expect_identical(dry_run_files, all_rmd)
  output <- capture_messages(
    built_files <-
      wflow_commit(all = TRUE, commit_files = new_file, path = site_dir))
  expect_identical(built_files, all_rmd)
  expect_identical(basename(all_rmd),
                   stringr::str_split(output[output != ""], " ",
                                      simplify = TRUE)[, 2] %>%
                     stringr::str_replace_all(., "\n", ""))
  expect_true(all(file.exists(all_html)))
  log <- commits(r)
  expect_identical(log[[1]]@message, "Build site.")
  expect_identical(log[[2]]@message, "Files commited by wflow_commit.")
  # Only the specified file should be in penultimate commit
  files_penultimate <- workflowr:::obtain_files_in_commit(r, log[[2]])
  files_penultimate <- file.path(site_dir, files_penultimate)
  expect_identical(new_file, files_penultimate)
  # Only the analysis HTML (which have the SHA1) should be in ultimate commit,
  # not pages like about.html.
  files_ultimate <- workflowr:::obtain_files_in_commit(r, log[[1]])
  files_ultimate <- file.path(site_dir, files_ultimate)
  expect_identical(test_html, files_ultimate)
  # All these files should have the penultimate commit ID inserted
  commit_penultimate <- extract_commit(path = site_dir, num = 2)$sha1
  for (html in test_html) {
    lines <- readLines(html)
    expect_true(any(grepl(commit_penultimate, lines)))
  }
})

unlink(site_dir, recursive = TRUE)
