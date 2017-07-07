context("windows")

# Setup ------------------------------------------------------------------------

os <- .Platform$OS.type
tmp_forw <- workflowr:::tempfile("windows-",
                                 tmpdir = workflowr:::normalizePath("/tmp"))
tmp_back <- stringr::str_replace_all(tmp_forw, "/", "\\\\")
rmd_forw <- "analysis/index.Rmd"
rmd_back <- "analysis\\index.Rmd"
html_forw <- "docs/index.html"
log_forw <- paste0(tmp_forw, "/log")
log_back <- paste0(tmp_forw, "\\log")
cwd <- getwd()

# workflowr user-facing functions:
# wflow_start
# wflow_open
# wflow_build
# wflow_commit
# wflow_publish
# wflow_status
# wflow_view
# extract_commit
# wflow_remove
# create_links_page
# wflow_remotes

# Can't test filepaths on these functions b/c they call `diff` utility:
# wflow_convert
# wflow_update

# Test forward slash -----------------------------------------------------------

test_that("workflowr functions can handle / on Windows", {

  if (os != "windows") skip("Only relevant on Windows")

  on.exit(setwd(cwd), add = TRUE)
  on.exit(unlink(tmp_forw, recursive = TRUE, force = TRUE), add = TRUE)

  # wflow_start
  expect_message(return_start <- wflow_start(tmp_forw),
                 tmp_forw)
  expect_identical(return_start, tmp_forw)
  # wflow_open
  return_open <- wflow_open(rmd_forw, change_wd = FALSE, open_file = FALSE)
  expect_identical(return_open, rmd_forw)
  # wflow_build
  utils::capture.output(return_build <- wflow_build(rmd_forw, log_dir = log_forw))
  expect_identical(return_build$files, rmd_forw)
  # wflow_commit
  return_commit <- wflow_commit(rmd_forw, dry_run = TRUE)
  expect_identical(return_commit$files, rmd_forw)
  # wflow_publish
  return_publish <- wflow_publish(rmd_forw, dry_run = TRUE)
  expect_identical(return_publish$step2$files, rmd_forw)
  expect_true(html_forw %in% return_publish$step3$files)
  # wflow_status
  return_status <- wflow_status(rmd_forw)
  expect_identical(rownames(return_status$status), rmd_forw)
  # wflow_view
  return_view <- wflow_view(rmd_forw, dry_run = TRUE)
  expect_identical(return_view, html_forw)
  # extract_commit
  recent_commit <- extract_commit(tmp_forw, 1)
  expect_false(is.na(recent_commit$sha1))
  # wflow_remove
  return_remove <- wflow_remove(rmd_forw, dry_run = TRUE)
  expect_identical(return_remove$files, c(rmd_forw, html_forw))
  # create_links_page
  expect_silent(create_links_page(project = tmp_forw))
  # wflow_remotes
  expect_silent(wflow_remotes(verbose = FALSE, project = tmp_forw))
})

# Test backslash -----------------------------------------------------------

test_that("workflowr functions can handle \ on Windows", {

  if (os != "windows") skip("Only relevant on Windows")

  on.exit(setwd(cwd), add = TRUE)
  on.exit(unlink(tmp_forw, recursive = TRUE, force = TRUE), add = TRUE)

  # wflow_start
  expect_message(return_start <- wflow_start(tmp_back),
                 tmp_forw)
  expect_identical(return_start, tmp_forw)
  # wflow_open
  return_open <- wflow_open(rmd_back, change_wd = FALSE, open_file = FALSE)
  expect_identical(return_open, rmd_forw)
  # wflow_build
  utils::capture.output(return_build <- wflow_build(rmd_back, log_dir = log_back))
  expect_identical(return_build$files, rmd_forw)
  # wflow_commit
  return_commit <- wflow_commit(rmd_back, dry_run = TRUE)
  expect_identical(return_commit$files, rmd_forw)
  # wflow_publish
  return_publish <- wflow_publish(rmd_back, dry_run = TRUE)
  expect_identical(return_publish$step2$files, rmd_forw)
  expect_true(html_forw %in% return_publish$step3$files)
  # wflow_status
  return_status <- wflow_status(rmd_back)
  expect_identical(rownames(return_status$status), rmd_forw)
  # wflow_view
  return_view <- wflow_view(rmd_back, dry_run = TRUE)
  expect_identical(return_view, html_forw)
  # extract_commit
  recent_commit <- extract_commit(tmp_back, 1)
  expect_false(is.na(recent_commit$sha1))
  # wflow_remove
  return_remove <- wflow_remove(rmd_back, dry_run = TRUE)
  expect_identical(return_remove$files, c(rmd_forw, html_forw))
  # create_links_page
  expect_silent(create_links_page(project = tmp_back))
  # wflow_remotes
  expect_silent(wflow_remotes(verbose = FALSE, project = tmp_back))
})


# Test diff utility ------------------------------------------------------------

test_that("workflowr fails gracefully if `diff` utility is not available", {

  if (os != "windows") skip("Only relevant on Windows")

  if (Sys.which("diff") != "") skip("Only relevant if Rtools not installed")

  on.exit(setwd(cwd), add = TRUE)
  on.exit(unlink(tmp_forw, recursive = TRUE, force = TRUE), add = TRUE)

  wflow_start(tmp_forw)

  expect_error(wflow_update(log_open = FALSE),
               "https://cran.r-project.org/bin/windows/Rtools/")
  expect_error(wflow_convert(rmd_forw),
               "https://cran.r-project.org/bin/windows/Rtools/")
})

# Test relpath -----------------------------------------------------------------

test_that("relpath works on Windows paths with trailing slashes", {
  path <- "C:/Users/home/myproject/analysis/"
  start <- "C:/Users/home/myproject/"
  expected <- "analysis"
  actual <- workflowr:::relpath(path, start)
  expect_identical(actual, expected)
})

test_that("relpath works on Windows paths without trailing slashes", {
  path <- "C:/Users/home/myproject/analysis"
  start <- "C:/Users/home/myproject"
  expected <- "analysis"
  actual <- workflowr:::relpath(path, start)
  expect_identical(actual, expected)
})

test_that("relpath works on Windows paths with mixmatched trailing slashes", {
  path <- "C:/Users/home/myproject/analysis"
  start <- "C:/Users/home/myproject/"
  expected <- "analysis"
  actual <- workflowr:::relpath(path, start)
  expect_identical(actual, expected)
})
