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

# Test wflow_update ------------------------------------------------------------

test_that("wflow_update can update from v0.3.0 to v0.4.0 with no Git", {

  if (skipping)
    skip("Must be run manually.")

  # Create a temporary directory with v0.3.0 files
  tmp_dir_v0.3.0 <- tempfile("v0.3.0-", tmpdir = normalizePath("/tmp"))
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

test_that("wflow_update can update from v0.3.0 to v0.4.0 with Git", {

  if (skipping)
    skip("Must be run manually.")

  # Create a temporary directory with v0.3.0 files
  tmp_dir_v0.3.0 <- tempfile("v0.3.0-", tmpdir = normalizePath("/tmp"))
  dir.create(tmp_dir_v0.3.0, recursive = TRUE)
  on.exit(unlink(tmp_dir_v0.3.0, recursive = TRUE))
  file.copy(from = "files/test-wflow_update/v0.3.0/.",
            to = tmp_dir_v0.3.0, recursive = TRUE)

  # Initilize Git repo and commit everything
  git2r::init(tmp_dir_v0.3.0)
  r <- git2r::repository(tmp_dir_v0.3.0)
  git2r::add(r, Sys.glob(file.path(tmp_dir_v0.3.0, "*")))
  git2r::commit(r, "commit project")
  commit_last <- git2r::commits(r)[[1]]

  # dry_run = TRUE
  expect_message(files_updated <- wflow_update(log_open = FALSE,
                                               project = tmp_dir_v0.3.0),
                 "Running wflow_update in dry run mode")
  expect_identical(git2r::commits(r)[[1]], commit_last)

  # dry_run = FALSE
  expect_message(files_updated <- wflow_update(dry_run = FALSE, log_open = FALSE,
                                               project = tmp_dir_v0.3.0),
                 "Running wflow_update")
  commit_update <- git2r::commits(r)[[1]]
  expect_identical(commit_update@message,
                   sprintf("Update workflowr project with wflow_update (version %s).",
                           as.character(utils::packageVersion("workflowr"))))
  files_expected <- workflowr:::obtain_files_in_commit(r, commit_update)
  files_expected <- file.path(tmp_dir_v0.3.0, files_expected)
  files_expected <- workflowr:::relpath_vec(files_expected)

  expect_false(commit_update@sha == commit_last@sha)
  expect_identical(sort(files_updated),
                   sort(files_expected))
})

test_that("wflow_update ignores Rmd files starting with _", {

  if (skipping)
    skip("Must be run manually.")

  # Create a temporary directory with v0.3.0 files
  tmp_dir_v0.3.0 <- tempfile("v0.3.0-", tmpdir = normalizePath("/tmp"))
  dir.create(tmp_dir_v0.3.0, recursive = TRUE)
  on.exit(unlink(tmp_dir_v0.3.0, recursive = TRUE))
  file.copy(from = "files/test-wflow_update/v0.3.0/.",
            to = tmp_dir_v0.3.0, recursive = TRUE)

  # Add an Rmd file starting with an underscore
  rmd_ignore <- file.path(tmp_dir_v0.3.0, "analysis", "_ignore.Rmd")
  file.create(rmd_ignore)
  rmd_ignore <- workflowr:::relpath(rmd_ignore)

  expect_message(files_updated <- wflow_update(dry_run = FALSE, log_open = FALSE,
                                               project = tmp_dir_v0.3.0),
                 "Running wflow_update")

  expect_true(length(files_updated) > 0)
  expect_false(rmd_ignore %in% files_updated)
})

test_that("wflow_update only commits tracked files", {

  if (skipping)
    skip("Must be run manually.")

  # Create a temporary directory with v0.3.0 files
  tmp_dir_v0.3.0 <- tempfile("v0.3.0-", tmpdir = normalizePath("/tmp"))
  dir.create(tmp_dir_v0.3.0, recursive = TRUE)
  on.exit(unlink(tmp_dir_v0.3.0, recursive = TRUE))
  file.copy(from = "files/test-wflow_update/v0.3.0/.",
            to = tmp_dir_v0.3.0, recursive = TRUE)

  # Initilize Git repo and commit everything
  git2r::init(tmp_dir_v0.3.0)
  r <- git2r::repository(tmp_dir_v0.3.0)
  git2r::add(r, Sys.glob(file.path(tmp_dir_v0.3.0, "*")))
  git2r::commit(r, "commit project")

  # Create an untracked Rmd file
  rmd_untracked <- file.path(tmp_dir_v0.3.0, "analysis", "untracked.Rmd")
  file.copy(file.path(tmp_dir_v0.3.0, "analysis", "ex1.Rmd"),
            rmd_untracked)
  rmd_untracked <- workflowr:::relpath(rmd_untracked)

  expect_message(files_updated <- wflow_update(dry_run = FALSE, log_open = FALSE,
                                               project = tmp_dir_v0.3.0),
                 "Running wflow_update")
  commit_update <- git2r::commits(r)[[1]]
  files_committed <- workflowr:::obtain_files_in_commit(r, commit_update)
  files_committed <- file.path(tmp_dir_v0.3.0, files_committed)
  files_committed <- workflowr:::relpath_vec(files_committed)

  expect_true(rmd_untracked %in% files_updated)
  expect_false(rmd_untracked %in% files_committed)
})

test_that("wflow_update does nothing if everything is up-to-date", {

  if (skipping)
    skip("Must be run manually.")

  # Create a temporary directory with v0.3.0 files
  tmp_dir_v0.3.0 <- tempfile("v0.3.0-", tmpdir = normalizePath("/tmp"))
  dir.create(tmp_dir_v0.3.0, recursive = TRUE)
  on.exit(unlink(tmp_dir_v0.3.0, recursive = TRUE))
  file.copy(from = "files/test-wflow_update/v0.3.0/.",
            to = tmp_dir_v0.3.0, recursive = TRUE)

  # Initilize Git repo and commit everything
  git2r::init(tmp_dir_v0.3.0)
  r <- git2r::repository(tmp_dir_v0.3.0)
  git2r::add(r, Sys.glob(file.path(tmp_dir_v0.3.0, "*")))
  git2r::commit(r, "commit project")
  commit_last <- git2r::commits(r)[[1]]

  # Update
  expect_message(files_updated <- wflow_update(dry_run = FALSE, log_open = FALSE,
                                               project = tmp_dir_v0.3.0),
                 "Running wflow_update")
  commit_update <- git2r::commits(r)[[1]]
  expect_false(commit_update@sha == commit_last@sha)

  # Run a second time
  expect_message(files_updated <- wflow_update(dry_run = FALSE, log_open = FALSE,
                                               project = tmp_dir_v0.3.0),
                 "Running wflow_update")
  commit_update_2 <- git2r::commits(r)[[1]]
  expect_true(commit_update@sha == commit_update_2@sha)
  expect_true(length(files_updated) == 0)
})

# Test error handling ----------------------------------------------------------

test_that("wflow_update fails early if files in staging area", {

  if (skipping)
    skip("Must be run manually.")

  # Create a temporary directory with v0.3.0 files
  tmp_dir_v0.3.0 <- tempfile("v0.3.0-", tmpdir = normalizePath("/tmp"))
  dir.create(tmp_dir_v0.3.0, recursive = TRUE)
  on.exit(unlink(tmp_dir_v0.3.0, recursive = TRUE))
  file.copy(from = "files/test-wflow_update/v0.3.0/.",
            to = tmp_dir_v0.3.0, recursive = TRUE)

  # Initilize Git repo and add everything
  git2r::init(tmp_dir_v0.3.0)
  r <- git2r::repository(tmp_dir_v0.3.0)
  git2r::add(r, Sys.glob(file.path(tmp_dir_v0.3.0, "*")))

  # Expect early error
  expect_error(wflow_update(log_open = FALSE, project = tmp_dir_v0.3.0),
               "You have added files to the Git staging area.")
})
