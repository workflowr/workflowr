context("commit")

library("git2r")

# start project in a tempdir
project_name <- "test-commit"
site_dir <- tempfile(paste0(project_name, "-"))
dir.create(site_dir)
suppressMessages(wflow_start(project_name, site_dir))
r <- repository(path = site_dir)

test_that("commit_site detects R Markdown files in root commit", {
  expect_identical(make_site(dry_run = TRUE, path = site_dir),
                   commit_site(dry_run = TRUE, path = site_dir))
})

# Commit site
suppressMessages(capture.output(commit_site(path = site_dir)))

test_that("commit_site creates HTML files", {
  html_files <- file.path(site_dir, "docs", c("about.html",
                                              "index.html",
                                              "license.html"))
  for (html in html_files) {
    expect_true(file.exists(html))
  }
})

log <- commits(r)

test_that("commit_site creates a new commit", {
  expect_identical(log[[1]]@message, "Build site.")
})

test_that("commit_site include .nojekyll in commit", {
  files_in_commit <- workflowr:::obtain_files_in_commit(r, log[[1]])
  expect_true("docs/.nojekyll" %in% files_in_commit)
})

# Add a new R Markdown file
# Can't use open_rmd b/c of the issue with devtools shims
# new_rmd <- open_rmd("test.Rmd", change_wd = FALSE, open_file = FALSE,
#                     path = site_dir)
new_rmd <- file.path(site_dir, "analysis", "test.Rmd")
file.create(new_rmd)
new_html <- file.path(site_dir, "docs", "test.html")

test_that("make_site, but not commit_site, renders untracked Rmd file", {
  make_site_files <- make_site(dry_run = TRUE, path = site_dir)
  commit_site_files <- commit_site(dry_run = TRUE, path = site_dir)
  expect_true(new_rmd %in% make_site_files)
  expect_false(new_rmd %in% commit_site_files)
})

# Stage file
add(r, new_rmd)

test_that("make_site, but not commit_site, renders staged Rmd file", {
  make_site_files <- make_site(dry_run = TRUE, path = site_dir)
  commit_site_files <- commit_site(dry_run = TRUE, path = site_dir)
  expect_true(new_rmd %in% make_site_files)
  expect_false(new_rmd %in% commit_site_files)
})

# Commit file
commit(r, new_rmd)

test_that("make_site **and** commit_site render committed Rmd file", {
  make_site_files <- make_site(dry_run = TRUE, path = site_dir)
  commit_site_files <- commit_site(dry_run = TRUE, path = site_dir)
  expect_true(new_rmd %in% make_site_files)
  expect_true(new_rmd %in% commit_site_files)
})

# Commit site
suppressMessages(capture.output(commit_site(path = site_dir)))

log <- commits(r)

test_that("commit_site includes latest html in commit, but not previous html", {
  files_in_commit <- workflowr:::obtain_files_in_commit(r, log[[1]])
  expect_true(file.path("docs", basename(new_html)) %in% files_in_commit)
  # Extract only files/directories in base of docs/
  files_in_docs <- sapply(strsplit(files_in_commit, split = "/"),
                          function(x) x[2])
  expect_true(sum(grepl("html$", files_in_docs)) == 1)
})

unlink(site_dir, recursive = TRUE)
