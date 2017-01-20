context("build")

# start project in a tempdir
project_name <- "test-build"
site_dir <- tempfile(paste0(project_name, "-"))
suppressMessages(wflow_start(project_name, site_dir, git = FALSE))

rmd_files <- list.files(path = file.path(site_dir, "analysis"),
                        pattern = "^[^_].*Rmd$",
                        full.names = TRUE)
stopifnot(length(rmd_files) > 0)
# Expected html files
html_files <- stringr::str_replace(rmd_files, "Rmd$", "html")
html_files <- stringr::str_replace(html_files, "/analysis/", "/docs/")

test_that("wflow_build detects R Markdown files to be built", {
  expect_identical(wflow_build(dry_run = TRUE, path = site_dir),
                   rmd_files)
  expect_message(wflow_build(dry_run = TRUE, path = site_dir),
                 "The following R Markdown files would be rendered:")
})

test_that("wflow_build builds HTML files", {
  output <- capture.output(built_files <- wflow_build(path = site_dir,
                                                      quiet = TRUE))
  expect_identical(rmd_files, built_files)
  expect_identical(rmd_files, stringr::str_split(output[output != ""], " ",
                                                 simplify = TRUE)[, 2])
  expect_true(all(file.exists(html_files)))
})

test_that("wflow_build does not build files unnecessarily", {
  html_mtime_pre <- file.mtime(html_files)
  expect_message(wflow_build(dry_run = TRUE, path = site_dir),
                 "All HTML files have been rendered")
  expect_message(wflow_build(path = site_dir),
                 "All HTML files have been rendered")
  expect_identical(wflow_build(path = site_dir),
                   character(0))
  html_mtime_post <- file.mtime(html_files)
  expect_identical(html_mtime_pre, html_mtime_post)
})

# Create some fake R Markdown files
# Unfortunately cannot use wflow_open here b/c of devtools
test_rmd <- file.path(site_dir, paste0("analysis/", 1:3, ".Rmd"))
for (i in 1:3) {
  file.copy("files/workflowr-template.Rmd", test_rmd[i])
}
# Expected html files
test_html <- stringr::str_replace(test_rmd, "Rmd$", "html")
test_html <- stringr::str_replace(test_html, "/analysis/", "/docs/")

test_that("wflow_build only builds new HTML files", {
  html_mtime_pre <- file.mtime(html_files)
  expect_identical(wflow_build(dry_run = TRUE, path = site_dir),
                   test_rmd)
  output <- capture.output(built_files <- wflow_build(path = site_dir,
                                                      quiet = TRUE))
  expect_identical(test_rmd, built_files)
  expect_identical(test_rmd, stringr::str_split(output[output != ""], " ",
                                                simplify = TRUE)[, 2])
  expect_true(all(file.exists(test_html)))
  html_mtime_post <- file.mtime(html_files)
  expect_identical(html_mtime_pre, html_mtime_post)
})

test_that("wflow_build builds one specified file", {
  html_mtime_pre <- file.mtime(test_html[1])
  expect_message(wflow_build(files = test_rmd[1], dry_run = TRUE,
                             path = site_dir),
                 "The following R Markdown files would be rendered:")
  expect_identical(wflow_build(files = test_rmd[1], dry_run = TRUE,
                               path = site_dir),
                   test_rmd[1])
  output <- capture.output(built_files <- wflow_build(files = test_rmd[1],
                                                      path = site_dir,
                                                      quiet = TRUE))
  expect_identical(test_rmd[1], built_files)
  expect_identical(test_rmd[1], stringr::str_split(output[output != ""], " ",
                                                   simplify = TRUE)[, 2])
  expect_true(all(file.exists(test_html[1])))
  html_mtime_post <- file.mtime(test_html[1])
  expect_true(html_mtime_pre < html_mtime_post)
})

test_that("wflow_build builds multiple specified files", {
  html_mtime_pre <- file.mtime(test_html[1:2])
  expect_message(wflow_build(files = test_rmd[1:2], dry_run = TRUE,
                             path = site_dir),
                 "The following R Markdown files would be rendered:")
  expect_identical(wflow_build(files = test_rmd[1:2], dry_run = TRUE,
                               path = site_dir),
                   test_rmd[1:2])
  output <- capture.output(built_files <- wflow_build(files = test_rmd[1:2],
                                                      path = site_dir,
                                                      quiet = TRUE))
  expect_identical(test_rmd[1:2], built_files)
  expect_identical(test_rmd[1:2], stringr::str_split(output[output != ""], " ",
                                                     simplify = TRUE)[, 2])
  expect_true(all(file.exists(test_html[1:2])))
  html_mtime_post <- file.mtime(test_html[1:2])
  expect_true(all(html_mtime_pre < html_mtime_post))
})

all_rmd <- sort(c(rmd_files, test_rmd))
all_html <- sort(c(html_files, test_html))

test_that("wflow_build can build 'all' files", {
  html_mtime_pre <- file.mtime(all_html)
  expect_message(wflow_build(all = TRUE, dry_run = TRUE,
                             path = site_dir),
                 "The following R Markdown files would be rendered:")
  expect_identical(wflow_build(all = TRUE, dry_run = TRUE,
                               path = site_dir),
                   all_rmd)
  output <- capture.output(built_files <- wflow_build(all = TRUE,
                                                      path = site_dir,
                                                      quiet = TRUE))
  expect_identical(all_rmd, built_files)
  expect_identical(all_rmd, stringr::str_split(output[output != ""], " ",
                                               simplify = TRUE)[, 2])
  expect_true(all(file.exists(all_html)))
  html_mtime_post <- file.mtime(all_html)
  expect_true(all(html_mtime_pre < html_mtime_post))
})

ignore_rmd <- file.path(site_dir, paste0("analysis/_ignore.Rmd"))
file.copy("files/workflowr-template.Rmd", ignore_rmd)
# Expected html files
ignore_html <- stringr::str_replace(ignore_rmd, "Rmd$", "html")
ignore_html <- stringr::str_replace(ignore_html, "/analysis/", "/docs/")

test_that("wflow_build ignores file starting with underscore", {
  capture.output(wflow_build(all = TRUE, path = site_dir))
  expect_false(file.exists(ignore_html))
})

test_that("wflow_build builds file starting with underscore if specified", {
  output <- capture.output(built_files <- wflow_build(files = ignore_rmd,
                                                      path = site_dir,
                                                      quiet = TRUE))
  expect_identical(ignore_rmd, built_files)
  expect_identical(ignore_rmd, stringr::str_split(output[output != ""], " ",
                                                  simplify = TRUE)[, 2])
  expect_true(file.exists(ignore_html))
})

unlink(site_dir, recursive = TRUE)
