context("windows")

# Setup ------------------------------------------------------------------------

os <- .Platform$OS.type
tmp_forw <- tempfile("windows-")
tmp_forw <- workflowr:::absolute(tmp_forw)
tmp_back <- stringr::str_replace_all(tmp_forw, "/", "\\\\")
rmd_forw <- "analysis/index.Rmd"
rmd_back <- "analysis\\index.Rmd"
html_forw <- "docs/index.html"
log_forw <- paste0(tmp_forw, "/log")
log_back <- paste0(tmp_forw, "\\log")
cwd <- getwd()

# workflowr user-facing functions:
# wflow_start
# wflow_build
# wflow_git_commit
# wflow_publish
# wflow_status
# wflow_view
# extract_commit
# wflow_remove
# wflow_git_remote

# Test forward slash -----------------------------------------------------------

test_that("workflowr functions can handle / on Windows", {

  if (os != "windows") skip("Only relevant on Windows")

  on.exit(setwd(cwd), add = TRUE)
  on.exit(unlink(tmp_forw, recursive = TRUE, force = TRUE), add = TRUE)

  # wflow_start
  return_start <- wflow_start(tmp_forw, user.name = "Test Name",
                              user.email = "test@email")
  expect_identical(return_start$directory, tmp_forw)
  # wflow_git_commit
  return_commit <- wflow_git_commit(rmd_forw, dry_run = TRUE)
  expect_identical(return_commit$files, rmd_forw)
  # wflow_status
  return_status <- wflow_status(rmd_forw)
  expect_identical(rownames(return_status$status), rmd_forw)
  # extract_commit
  recent_commit <- extract_commit(tmp_forw, 1)
  expect_false(is.na(recent_commit$sha1))
  # wflow_git_remote
  expect_silent(wflow_git_remote(verbose = FALSE, project = tmp_forw))

  skip_on_cran()

  # wflow_build
  utils::capture.output(return_build <- wflow_build(rmd_forw, view = FALSE,
                                                    log_dir = log_forw))
  expect_identical(return_build$files, rmd_forw)
  # wflow_publish
  return_publish <- wflow_publish(rmd_forw, dry_run = TRUE)
  expect_identical(return_publish$step2$files, rmd_forw)
  expect_true(html_forw %in% return_publish$step3$files)
  # wflow_view
  return_view <- wflow_view(rmd_forw, dry_run = TRUE)
  expect_identical(return_view$opened, html_forw)
  # wflow_remove
  return_remove <- wflow_remove(rmd_forw, dry_run = TRUE)
  expect_identical(return_remove$files, c(rmd_forw, html_forw))
})

# Test backslash -----------------------------------------------------------

test_that("workflowr functions can handle \\ on Windows", {

  if (os != "windows") skip("Only relevant on Windows")

  on.exit(setwd(cwd), add = TRUE)
  on.exit(unlink(tmp_forw, recursive = TRUE, force = TRUE), add = TRUE)

  # wflow_start
  return_start <- wflow_start(tmp_back, user.name = "Test Name",
                              user.email = "test@email")
  expect_identical(return_start$directory, tmp_forw)
  # wflow_git_commit
  return_commit <- wflow_git_commit(rmd_back, dry_run = TRUE)
  expect_identical(return_commit$files, rmd_forw)
  # wflow_status
  return_status <- wflow_status(rmd_back)
  expect_identical(rownames(return_status$status), rmd_forw)
  # extract_commit
  recent_commit <- extract_commit(tmp_back, 1)
  expect_false(is.na(recent_commit$sha1))
  # wflow_git_remote
  expect_silent(wflow_git_remote(verbose = FALSE, project = tmp_back))

  skip_on_cran()

  # wflow_build
  utils::capture.output(return_build <- wflow_build(rmd_back, view = FALSE,
                                                    log_dir = log_back))
  expect_identical(return_build$files, rmd_forw)
  # wflow_publish
  return_publish <- wflow_publish(rmd_back, dry_run = TRUE)
  expect_identical(return_publish$step2$files, rmd_forw)
  expect_true(html_forw %in% return_publish$step3$files)
  # wflow_view
  return_view <- wflow_view(rmd_back, dry_run = TRUE)
  expect_identical(return_view$opened, html_forw)
  # wflow_remove
  return_remove <- wflow_remove(rmd_back, dry_run = TRUE)
  expect_identical(return_remove$files, c(rmd_forw, html_forw))
})

# Test absolute ----------------------------------------------------------------

test_that("absolute can handle the Windows drive", {

  if (os != "windows") skip("Only relevant on Windows")

  drive <- Sys.getenv("HOMEDRIVE")

  # Can't do this test if HOMEDRIVE is not set (e.g. on r-hub)
  if (drive == "") skip("HOMEDRIVE not set")

  drive_back <- paste0(drive, "\\")
  drive_forw <- paste0(drive, "/")
  expect_identical(drive_forw, workflowr:::absolute(drive_back))
  expect_identical(drive_forw, workflowr:::absolute(drive_forw))
})

# Test relative -----------------------------------------------------------------

test_that("relative works on Windows paths with trailing slashes", {

  if (os != "windows") skip("Only relevant on Windows")

  path <- "C:/Users/home/myproject/analysis/"
  start <- "C:/Users/home/myproject/"
  expected <- "analysis"
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative works on Windows paths without trailing slashes", {

  if (os != "windows") skip("Only relevant on Windows")

  path <- "C:/Users/home/myproject/analysis"
  start <- "C:/Users/home/myproject"
  expected <- "analysis"
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative works on Windows paths with mixmatched trailing slashes", {

  if (os != "windows") skip("Only relevant on Windows")

  path <- "C:/Users/home/myproject/analysis"
  start <- "C:/Users/home/myproject/"
  expected <- "analysis"
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

# Test get_home ----------------------------------------------------------------

test_that("get_home obtains a valid home directory", {
  home <- workflowr:::get_home()
  expect_true(fs::dir_exists(home))
  expect_false(stringr::str_detect(home, "Documents"))
})
