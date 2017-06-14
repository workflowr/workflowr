# Can't run with devtools_shims b/c need to access package files
if ("devtools_shims" %in% search()) {
  skipping <- TRUE
} else {
  skipping <- FALSE
  message("Running tests in test-wflow_update.R")
  if (!file.exists("test-wflow_update.R"))
    stop("Tests must be manually run in same working directory as this file.",
         call. = FALSE)
  library("testthat")
  library("workflowr")
}

context("wflow_update")

test_that("Can update from v0.3.0 to v0.4.0", {

  if (skipping)
    skip("Must be run manually.")

  # Create a temporary directory with v0.3.0 files
  tmp_dir_v0.3.0 <- tempfile("v0.3.0-")
  dir.create(tmp_dir_v0.3.0, recursive = TRUE)
  on.exit(unlink(tmp_dir_v0.3.0, recursive = TRUE))
  file.copy(from = "files/test-wflow_update/v0.3.0/.",
            to = tmp_dir_v0.3.0, recursive = TRUE)
  # dry_run = TRUE
  expect_message(files_updated <- wflow_update(log_open = FALSE,
                                               project = tmp_dir_v0.3.0),
                 "Running wflow_update in dry run mode")
  files_expected <- c(file.path(tmp_dir_v0.3.0, "v0.3.0.Rproj"),
                      file.path(tmp_dir_v0.3.0, "analysis",
                                c("chunks.R", "ex1.Rmd", "ex2.Rmd")))
  files_expected <- workflowr:::relpath_vec(files_expected)
  expect_identical(sort(files_updated), sort(files_expected))
  # dry_run = FALSE
  expect_message(wflow_update(dry_run = FALSE, log_open = FALSE,
                              project = tmp_dir_v0.3.0),
                 "Running wflow_update")
  files_expected <- list.files("files/test-wflow_update/v0.4.0/",
                               full.names = TRUE, recursive = TRUE)
  files_actual <- list.files(tmp_dir_v0.3.0,
                             full.names = TRUE, recursive = TRUE)
  files_expected_md5 <- tools::md5sum(files_expected)
  files_actual_md5 <- tools::md5sum(files_actual)
  expect_true(all(files_actual_md5 == files_expected_md5))
})
