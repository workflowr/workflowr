context("wflow_publish")

# Setup ------------------------------------------------------------------------

library("git2r")

# Setup workflowr project for testing
site_dir <- tempfile("test-wflow_publish-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE))
# Delete workflowr project on exit
on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))
site_dir <- workflowr:::absolute(site_dir)
s <- wflow_status(project = site_dir)
r <- repository(s$root)

rmd <- file.path(s$analysis, c("about.Rmd", "index.Rmd", "license.Rmd"))
html <- workflowr:::to_html(rmd, outdir = s$docs)

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

# Create decoy file that should not be built since it is unpublished
rmd_decoy <- file.path(s$analysis, "decoy.Rmd")
file.create(rmd_decoy)
html_decoy <- workflowr:::to_html(rmd_decoy, outdir = s$docs)

test_that("wflow_publish can `republish`", {
  mtime_pre <- file.mtime(html)
  Sys.sleep(2)
  # Change the theme
  config <- file.path(s$analysis, "_site.yml")
  config_lines <- readLines(config)
  config_lines <- stringr::str_replace(config_lines,
                                       "    theme: cosmo",
                                       "    theme: readable")
  writeLines(config_lines, con = config)
  # Republish with new theme
  expect_message(o <- wflow_publish(config, republish = TRUE,
                                    project = site_dir),
                 rmd[1])
  mtime_post <- file.mtime(html)
  expect_true(all(mtime_post > mtime_pre))
  expect_true(config == o$step1$commit_files)
  expect_true(all(html %in% o$step3$commit_files))
  expect_false(file.exists(html_decoy))
  expect_false(html_decoy %in% o$step3$commit_files)
})

# Commit decoy file. Should not be affected by `update = TRUE` b/c it has not
# been published.
wflow_commit(rmd_decoy, "Commit decoy Rmd", project = site_dir)

test_that("wflow_publish can `update`", {
  # Edit and manually commit a published Rmd file, then use `update` to publish.
  cat("edit", file = rmd[1], append = TRUE)
  wflow_commit(rmd[1], "Draft edit", project = site_dir)
  # Update
  expect_message(o <- wflow_publish(update = TRUE, project = site_dir),
                 rmd[1])
  expect_true(is.null(o$step1))
  expect_true(html[1] == o$step3$commit_files)
  expect_false(file.exists(html_decoy))
  expect_false(html_decoy %in% o$step3$commit_files)
})

test_that("wflow_publish can be used to commit non-Rmd files instead of wflow_commit", {
  f_test <- file.path(s$root, "test.txt")
  file.create(f_test)
  expect_silent(o <- wflow_publish(f_test, project = site_dir))
  expect_true(f_test == o$step1$commit_files)
  expect_true(is.null(o$step2))
  expect_true(is.null(o$step3))
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
  Sys.sleep(2)
  expect_error(utils::capture.output(
    wflow_publish(c(rmd, rmd_to_fail), project = site_dir)),
    "There was an error")
  md5sum_post <- tools::md5sum(rmd)
  mtime_post <- file.mtime(rmd)
  expect_identical(md5sum_post, md5sum_pre)
  expect_identical(mtime_post, mtime_pre)
})
