context("wflow_update")

# Test wflow_update ------------------------------------------------------------

test_that("wflow_update can update to workflowr 1.0", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  file.copy("files/test-wflow_update/pre/.", tmp_dir, recursive = TRUE)
  dir.create(file.path(tmp_dir, "docs"))
  git2r::init(tmp_dir)
  r <- git2r::repository(tmp_dir)
  git2r::add(r, ".")
  git2r::commit(r, "Initial commit.")

  # Dry run
  expected <- file.path(tmp_dir, c("_workflowr.yml", "analysis/_site.yml",
                                   "analysis/chunks.R",
                                   "analysis/include/footer.html",
                                   "analysis/index.Rmd",
                                   "analysis/workflowr-template.Rmd"))
  files_dry <- wflow_update(project = tmp_dir)
  expect_identical(workflowr:::absolute(files_dry), expected)

  # Update the files
  files_updated <- wflow_update(dry_run = FALSE, project = tmp_dir)
  expect_identical(workflowr:::absolute(files_updated), expected)

  # Confirm files are updated correctly
  lines_expected <- Map(readLines, list.files("files/test-wflow_update/post",
                                              recursive = TRUE, full.names = TRUE))
  names(lines_expected) <- NULL
  lines_observed <- Map(readLines, list.files(tmp_dir, recursive = TRUE,
                                              full.names = TRUE))
  names(lines_observed) <- NULL
  expect_identical(lines_observed, lines_expected)

  # Confirm that all files can be committed easily
  git_commit <- wflow_git_commit(file.path(tmp_dir, "_workflowr.yml"),
                                 "Update to 1.0", all = TRUE, project = tmp_dir)
  expect_identical("Update to 1.0", git_commit$message)
  expect_identical(workflowr:::absolute(git_commit$commit_files), expected)

  # Confirm that subsequent calls to wflow_update have no effect
  files_dry_post <- wflow_update(project = tmp_dir)
  expect_identical(files_dry_post, character(0))
  files_updated_post <- wflow_update(dry_run = FALSE, project = tmp_dir)
  expect_identical(files_updated_post, character(0))
  s <- git2r::status(r)
  expect_null(unlist(s))
})
