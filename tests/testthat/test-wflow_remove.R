context("wflow_remove")

# Setup -----------------------------------------------------------------------

library("git2r")
cwd <- getwd()
tdir <- tempfile("test-wflow_remove-")
on.exit(setwd(cwd))
on.exit(unlink(tdir, recursive = TRUE, force = TRUE), add = TRUE)
suppressMessages(wflow_start(tdir, user.name = "Test Name",
                             user.email = "test@email"))
tdir <- workflowr:::absolute(tdir)
r <- repository(path = tdir)
p <- wflow_paths()

chunk_w_plot <- c("",
                  "```{r a-plot, cache=TRUE}",
                  "plot(1:10)",
                  "```",
                  "")

# Skip on CRAN. See ?testthat::skip_on_cran, which only works inside of unit
# test functions.
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  # Create an Rmd and a data file to be removed later. Add a chunk with a plot and
  # that is cached. Publish this analysis.
  rmd_published <- file.path(p$analysis, "published.Rmd")
  file.copy(from = file.path(cwd, "files", "example.Rmd"),
            to = rmd_published)
  cat(chunk_w_plot, file = rmd_published, sep = "\n", append = TRUE)
  data_published <- file.path("data", "published.txt")
  file.create(data_published)
  suppressMessages(x <- wflow_publish(c(rmd_published, data_published), view = FALSE))
  cache_published <- file.path(p$analysis,
                               paste0(tools::file_path_sans_ext(
                                 basename(rmd_published)), "_cache"))
  fig_analysis_published <- file.path(p$analysis, "figure", basename(rmd_published))
  fig_docs_published <- file.path(p$docs, "figure", basename(rmd_published))

  # Create an Rmd and a data file to be removed later. Add a chunk with a plot and
  # that is cached. Build but do not publish this analysis.
  rmd_unpublished <- file.path(p$analysis, "unpublished.Rmd")
  file.copy(from = rmd_published, to = rmd_unpublished)
  data_unpublished <- file.path("data", "unpublished.txt")
  file.create(data_unpublished)
  suppressMessages(x <- wflow_build(rmd_unpublished, view = FALSE))
  cache_unpublished <- file.path(p$analysis,
                                 paste0(tools::file_path_sans_ext(
                                   basename(rmd_unpublished)), "_cache"))
  fig_analysis_unpublished <- file.path(p$analysis, "figure", basename(rmd_unpublished))
  fig_docs_unpublished <- file.path(p$docs, "figure", basename(rmd_unpublished))
}

# Test wflow_remove ------------------------------------------------------------

test_that("wflow_remove removes an unpublished Rmd file and its associated files", {

  skip_on_cran()

  # First a dry run
  actual <- wflow_remove(c(rmd_unpublished, data_unpublished), dry_run = TRUE)
  expect_true(all(c(rmd_unpublished, data_unpublished) %in% actual$files))
  expect_identical(actual$message,
                   "wflow_remove(c(rmd_unpublished, data_unpublished), dry_run = TRUE)")
  expect_identical(actual$dry_run, TRUE)
  expect_identical(actual$commit, NA)
  expect_identical(actual$files_git, character())
  expect_true(all(file.exists(rmd_unpublished, data_unpublished),
                  dir.exists(c(cache_unpublished, fig_docs_unpublished))))
  # Now remove the files
  actual <- wflow_remove(c(rmd_unpublished, data_unpublished))
  expect_false(any(file.exists(rmd_unpublished, data_unpublished),
                  dir.exists(c(cache_unpublished, fig_docs_unpublished))))
})

test_that("wflow_remove removes a published Rmd file and its associated files", {

  skip_on_cran()

  # First a dry run
  actual <- wflow_remove(c(rmd_published, data_published), dry_run = TRUE)
  expect_true(all(c(rmd_published, data_published) %in% actual$files))
  expect_identical(actual$message,
                   "wflow_remove(c(rmd_published, data_published), dry_run = TRUE)")
  expect_identical(actual$dry_run, TRUE)
  expect_identical(actual$commit, NA)
  expect_true(all(c(rmd_published, data_published) %in% actual$files_git))
  expect_true(all(file.exists(rmd_published, data_published),
                  dir.exists(c(cache_published, fig_docs_published))))
  files_committed <- workflowr:::get_committed_files(r)
  expect_true(all(c(rmd_published, data_published) %in% files_committed))
  # Now remove the files
  actual <- wflow_remove(c(rmd_published, data_published))
  expect_false(any(file.exists(rmd_published, data_published),
                   dir.exists(c(cache_published, fig_docs_published))))
  commit_latest <- commits(r)[[1]]
  expect_identical(actual$commit@sha, commit_latest@sha)
  expect_identical(commit_latest@message,
                   "wflow_remove(c(rmd_published, data_published))")
  # Confirm the files were removed from the Git directory
  files_committed <- workflowr:::get_committed_files(r)
  expect_false(any(c(rmd_published, data_published) %in% files_committed))
})

test_that("wflow_remove can remove files with no Git repo present", {
  # Temporarily move .git directory
  tgit <- tempfile("git-")
  on.exit(file.rename(from = tgit, to = p$git), add = TRUE)
  file.rename(from = p$git, to = tgit)
  tgit <- workflowr:::absolute(tgit)
  # The test will remove README, so restore it afterwards
  f <- "README.md"
  on.exit(checkout(r, path = f), add = TRUE)
  expect_true(file.exists(f))
  # Remove README.md
  expect_silent(actual <- wflow_remove(f))
  expect_false(file.exists(f))
  expect_identical(actual$files, f)
  expect_identical(actual$commit, NA)
  expect_identical(actual$files_git, NA)
})

test_that("wflow_remove can remove a directory", {
  d <- "toplevel"
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE, force = TRUE))
  f <- file.path(d, "file")
  file.create(f)
  add(r, f)
  commit(r, "new file")
  actual <- wflow_remove(d)
  expect_identical(actual$files_git, f)
  expect_false(dir.exists(d))
  expect_false(file.exists(f))
})

# Test error handling ----------------------------------------------------------

test_that("wflow_remove requires valid argument: files", {
  expect_error(wflow_remove(1),
               "files must be a character vector of filenames")
  expect_error(wflow_remove(NA),
               "files must be a character vector of filenames")
  expect_error(wflow_remove(NULL),
               "files must be a character vector of filenames")
  expect_error(wflow_remove(TRUE),
               "files must be a character vector of filenames")
  expect_error(wflow_remove("nonexistent.Rmd"),
               "Not all files exist. Check the paths to the files")
})

test_that("wflow_remove requires valid argument: message", {
  expect_error(wflow_remove("analysis/index.Rmd", message = 1, dry_run = TRUE),
               "message must be NULL or a character vector")
  expect_error(wflow_remove("analysis/index.Rmd", message = NA, dry_run = TRUE),
               "message must be NULL or a character vector")
  expect_error(wflow_remove("analysis/index.Rmd", message = TRUE, dry_run = TRUE),
               "message must be NULL or a character vector")
})

test_that("wflow_remove requires valid argument: git", {
  expect_error(wflow_remove("analysis/index.Rmd", git = 1, dry_run = TRUE),
               "git must be a one-element logical vector")
  # R intercepts NA error at beginning of function call
  expect_error(wflow_remove("analysis/index.Rmd", git = NA, dry_run = TRUE),
               "missing value where TRUE/FALSE needed")
  expect_error(wflow_remove("analysis/index.Rmd", git = NULL, dry_run = TRUE),
               "git must be a one-element logical vector")
  expect_error(wflow_remove("analysis/index.Rmd", git = "TRUE", dry_run = TRUE),
               "git must be a one-element logical vector")
  expect_error(wflow_remove("analysis/index.Rmd", git = c(TRUE, TRUE),
                            dry_run = TRUE),
               "git must be a one-element logical vector")
})

test_that("wflow_remove requires valid argument: dry_run", {
  expect_error(wflow_remove("analysis/index.Rmd", dry_run = 1),
               "dry_run must be a one-element logical vector")
  # R intercepts NA error at beginning of function call
  expect_error(wflow_remove("analysis/index.Rmd", dry_run = NA),
               "missing value where TRUE/FALSE needed")
  expect_error(wflow_remove("analysis/index.Rmd", dry_run = NULL),
               "dry_run must be a one-element logical vector")
  expect_error(wflow_remove("analysis/index.Rmd", dry_run = "TRUE"),
               "dry_run must be a one-element logical vector")
  expect_error(wflow_remove("analysis/index.Rmd", dry_run = c(TRUE, TRUE)),
               "dry_run must be a one-element logical vector")
})

test_that("wflow_remove requires valid argument: project", {
  expect_error(wflow_remove("analysis/index.Rmd", project = 1, dry_run = TRUE),
               "project must be a one-element character vector")
  expect_error(wflow_remove("analysis/index.Rmd", project = NA, dry_run = TRUE),
               "project must be a one-element character vector")
  expect_error(wflow_remove("analysis/index.Rmd", project = NULL, dry_run = TRUE),
               "project must be a one-element character vector")
  expect_error(wflow_remove("analysis/index.Rmd", project = TRUE, dry_run = TRUE),
               "project must be a one-element character vector")
  expect_error(wflow_remove("analysis/index.Rmd", project = c("a", "b"),
                            dry_run = TRUE),
               "project must be a one-element character vector")
  expect_error(wflow_remove("analysis/index.Rmd", project = "nonexistent/",
                            dry_run = TRUE),
               "project directory does not exist.")
})
