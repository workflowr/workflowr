context("wflow_publish")

# Setup ------------------------------------------------------------------------

library("git2r")

# Setup workflowr project for testing
site_dir <- tempfile("test-wflow_publish-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE))
# Delete workflowr project on exit
on.exit(unlink(site_dir, recursive = TRUE))
s <- wflow_status(project = site_dir)
r <- repository(s$root)

rmd <- file.path(s$analysis, c("about.Rmd", "index.Rmd", "license.Rmd"))
html <- to_html(rmd, outdir = s$docs)

rmd_to_fail <- file.path(s$analysis, "error.Rmd")
file.copy(from = "files/test-wflow_build/error.Rmd",
          to = rmd_to_fail)

# Test wflow_publish -----------------------------------------------------------

test_that("wflow_publish works in a simple case", {
  expect_message(o <- wflow_publish(rmd, project = site_dir),
                 rmd[1])
  expect_true(all(file.exists(html)))
  s <- wflow_status(project = site_dir)
  expect_true(all(s$status[rmd, "published"]))
})

# Test error handling ----------------------------------------------------------

test_that("wflow_publish resets Git repo to previous commit if build fails", {
  commit_pre <- commits(r, n = 1)[[1]]
  expect_error(utils::capture.output(
    wflow_publish(rmd_to_fail, project = site_dir)),
               "There was an error")
  commit_post <- commits(r, n = 1)[[1]]
  expect_identical(commit_post, commit_pre)
})

test_that("wflow_publish restores previous docs/ if build fails", {
  md5sum_pre <- tools::md5sum(rmd)
  mtime_pre <- file.mtime(rmd)
  expect_error(utils::capture.output(
    wflow_publish(c(rmd, rmd_to_fail), project = site_dir)),
    "There was an error")
  md5sum_post <- tools::md5sum(rmd)
  mtime_post <- file.mtime(rmd)
  expect_identical(md5sum_post, md5sum_pre)
  expect_identical(mtime_post, mtime_pre)
})
