context("vig-getting-started")

# Testing similar workflow as in getting-started vignette

library("git2r")

# start project in a tempdir
site_dir <- tempfile("new-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE, user.name = "Test Name",
                             user.email = "test@email"))
on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))
site_dir <- workflowr:::absolute(site_dir)
r <- repository(path = site_dir)
s <- wflow_status(project = site_dir)

test_that("wflow_start provides necessary infrastructure", {
  expect_true(fs::dir_exists(file.path(site_dir, ".git")))
  expect_true(fs::dir_exists(file.path(site_dir, "analysis")))
  expect_true(fs::file_exists(file.path(site_dir, "analysis/_site.yml")))
  expect_true(fs::file_exists(file.path(site_dir, "analysis/index.Rmd")))
  expect_true(fs::file_exists(file.path(site_dir,
                                    paste0(basename(site_dir), ".Rproj"))))
  expect_true(length(commits(r)) == 1)
})

rmd <- rownames(s$status)
stopifnot(length(rmd) > 0)
# Expected html files
html <- workflowr:::to_html(rmd, outdir = s$docs)

test_that("wflow_build builds the website, but only once", {

  skip_on_cran()

  suppressMessages(o <- wflow_build(view = FALSE, project = site_dir))
  expect_identical(o$built, rmd)
  expect_true(all(fs::file_exists(html)))
  expect_silent(o <- wflow_build(view = FALSE, project = site_dir))
})

test_that("wflow_view opens website.", {

  skip_on_cran()

  expected <- file.path(s$docs, "index.html")
  actual <- wflow_view(dry_run = TRUE, project = site_dir)
  expect_identical(actual$opened, expected)
})

test_rmd <- file.path(s$analysis, "first-analysis.Rmd")
fs::file_copy("files/example.Rmd", test_rmd)
# Expected html file
test_html <- workflowr:::to_html(test_rmd, outdir = s$docs)
s <- wflow_status(project = site_dir)

test_that("wflow_build only builds new file", {

  skip_on_cran()

  html_mtime_pre <- file.mtime(html)
  Sys.sleep(2)
  suppressMessages(o <- wflow_build(view = FALSE, project = site_dir))
  expect_identical(o$built, test_rmd)
  expect_true(fs::file_exists(test_html))
  html_mtime_post <- file.mtime(html)
  expect_identical(html_mtime_pre, html_mtime_post)
  expect_silent(wflow_build(view = FALSE, project = site_dir))
})

test_that("wflow_view can open specific file with Rmd extension.", {

  skip_on_cran()

  expected <- file.path(s$docs, "first-analysis.html")
  actual <- wflow_view(file.path(s$analysis, "first-analysis.Rmd"),
                       dry_run = TRUE, project = site_dir)
  expect_identical(actual$opened, expected)
})

all_rmd <- rownames(s$status)
all_html <- workflowr:::to_html(all_rmd, outdir = s$docs)
test_that("wflow_publish can commit new file and website", {

  skip_on_cran()

  html_mtime_pre <- file.mtime(all_html)
  Sys.sleep(2)
  expect_message(o <- wflow_publish(all_rmd,
                                    message = "first analysis",
                                    view = FALSE,
                                    project = site_dir))
  expect_identical(o$step2$built, all_rmd)
  expect_true(all(fs::file_exists(all_html)))
  html_mtime_post <- file.mtime(all_html)
  expect_true(all(html_mtime_pre < html_mtime_post))
  log <- commits(r)
  expect_true(length(log) == 3)
  expect_identical(git2r_slot(log[[1]], "message"), "Build site.")
  expect_identical(git2r_slot(log[[2]], "message"), "first analysis")
  expect_silent(wflow_build(make = TRUE, update = TRUE, view = FALSE,
                            project = site_dir))
})
