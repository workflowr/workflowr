context("wflow_remove")

# Setup ------------------------------------------------------------------------

library("git2r")

# Load helper function local_no_gitconfig()
source("helpers.R", local = TRUE)

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
  fs::file_copy(file.path(cwd, "files", "example.Rmd"), rmd_published)
  cat(chunk_w_plot, file = rmd_published, sep = "\n", append = TRUE)
  data_published <- file.path("data", "published.txt")
  fs::file_create(data_published)
  suppressMessages(x <- wflow_publish(c(rmd_published, data_published), view = FALSE))
  cache_published <- file.path(p$analysis,
                               paste0(tools::file_path_sans_ext(
                                 basename(rmd_published)), "_cache"))
  fig_analysis_published <- file.path(p$analysis, "figure", basename(rmd_published))
  fig_docs_published <- file.path(p$docs, "figure", basename(rmd_published))

  # Create an Rmd and a data file to be removed later. Add a chunk with a plot and
  # that is cached. Build but do not publish this analysis.
  rmd_unpublished <- file.path(p$analysis, "unpublished.Rmd")
  fs::file_copy(rmd_published, rmd_unpublished)
  data_unpublished <- file.path("data", "unpublished.txt")
  fs::file_create(data_unpublished)
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
  expect_true(all(fs::file_exists(c(rmd_unpublished, data_unpublished)),
                  fs::dir_exists(c(cache_unpublished, fig_docs_unpublished))))
  # Now remove the files
  actual <- wflow_remove(c(rmd_unpublished, data_unpublished))
  expect_false(any(fs::file_exists(c(rmd_unpublished, data_unpublished)),
                  fs::dir_exists(c(cache_unpublished, fig_docs_unpublished))))
  # Test print method
  expect_output(print(actual), rmd_unpublished)
  expect_output(print(actual), data_unpublished)
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
  expect_true(all(fs::file_exists(c(rmd_published, data_published)),
                  fs::dir_exists(c(cache_published, fig_docs_published))))
  files_committed <- workflowr:::get_committed_files(r)
  expect_true(all(c(rmd_published, data_published) %in% relative(files_committed)))
  # Now remove the files
  actual <- wflow_remove(c(rmd_published, data_published))
  expect_false(any(fs::file_exists(c(rmd_published, data_published)),
                   fs::dir_exists(c(cache_published, fig_docs_published))))
  # Test print method
  expect_output(print(actual), rmd_published)
  expect_output(print(actual), data_published)
  # Confirm a commit was made
  commit_latest <- commits(r)[[1]]
  expect_identical(git2r_slot(actual$commit, "sha"),
                   git2r_slot(commit_latest, "sha"))
  expect_identical(git2r_slot(commit_latest, "message"),
                   "wflow_remove(c(rmd_published, data_published))")
  # Confirm the files were removed from the Git directory
  files_committed <- workflowr:::get_committed_files(r)
  expect_false(any(c(rmd_published, data_published) %in% files_committed))
})

test_that("wflow_remove can remove files with no Git repo present", {
  # Temporarily move .git directory
  tgit <- tempfile("git-")
  on.exit(file.rename(from = tgit, to = file.path(p$git, ".git")), add = TRUE)
  file.rename(from = file.path(p$git, ".git"), to = tgit)
  tgit <- workflowr:::absolute(tgit)
  # The test will remove README, so restore it afterwards
  f <- "README.md"
  on.exit(checkout(r, path = f), add = TRUE)
  expect_true(fs::file_exists(f))
  # Remove README.md
  expect_silent(actual <- wflow_remove(f))
  expect_false(fs::file_exists(f))
  expect_identical(actual$files, f)
  expect_identical(actual$commit, NA)
  expect_identical(actual$files_git, NA)
})

test_that("wflow_remove can remove a directory", {
  d <- "toplevel"
  fs::dir_create(d)
  on.exit(unlink(d, recursive = TRUE, force = TRUE))
  f <- file.path(d, "file")
  fs::file_create(f)
  add(r, f)
  commit(r, "new file")
  actual <- wflow_remove(d)
  expect_identical(actual$files_git, f)
  expect_false(fs::dir_exists(d))
  expect_false(fs::file_exists(f))
})

# Test needed to handle Windows behavior of fs::file_exists
test_that("wflow_remove can remove a directory with a trailing slash", {
  d <- "toplevel/"
  fs::dir_create(d)
  on.exit(unlink(d, recursive = TRUE, force = TRUE))
  f <- file.path(d, "file")
  fs::file_create(f)
  add(r, f)
  commit(r, "new file")
  actual <- wflow_remove(d)
  expect_identical(actual$files_git, workflowr:::relative(f))
  expect_false(fs::dir_exists(d))
  expect_false(fs::file_exists(f))
})

test_that("wflow_remove can remove a file with a relative path from a subdir", {
  rmd <- file.path(p$analysis, "new.Rmd")
  fs::file_create(rmd)
  add(r, rmd)
  commit(r, "Add an rmd file to remove")

  expect_silent(with_dir("code/",
                         o <- wflow_remove(file.path("../analysis", basename(rmd)))))
  expect_false(fs::file_exists(rmd))
  s <- status(r)
  expect_equal(length(s$untracked) + length(s$unstaged) + length(s$staged), 0)
})

test_that("wflow_remove can remove figure file from analysis/", {
  rmd <- file.path(p$analysis, "new.Rmd")
  fs::file_create(rmd)
  dir_fig <- file.path(p$analysis, workflowr:::create_figure_path(rmd))
  fs::dir_create(dir_fig)
  fig <- file.path(dir_fig, "test.png")
  fs::file_create(fig)
  expect_silent(actual <- wflow_remove(rmd))
  expect_identical(actual$files, c(rmd, fig))
  expect_false(fs::file_exists(rmd))
  expect_false(fs::file_exists(fig))
  expect_false(fs::dir_exists(dir_fig))

  # Same thing, but from analysis/ subdirectory
  rmd <- file.path(p$analysis, "new.Rmd")
  fs::file_create(rmd)
  dir_fig <- file.path(p$analysis, workflowr:::create_figure_path(rmd))
  fs::dir_create(dir_fig)
  fig <- file.path(dir_fig, "test.png")
  fs::file_create(fig)
  expect_silent(with_dir(p$analysis,
                         actual <- wflow_remove(file.path("../analysis", basename(rmd)))))
  expect_false(fs::file_exists(rmd))
  expect_false(fs::file_exists(fig))
  expect_false(fs::dir_exists(dir_fig))
})

test_that("wflow_remove can remove figure file from docs/", {
  rmd <- file.path(p$analysis, "new.Rmd")
  fs::file_create(rmd)
  dir_fig <- file.path(p$docs, workflowr:::create_figure_path(rmd))
  fs::dir_create(dir_fig)
  fig <- file.path(dir_fig, "test.png")
  fs::file_create(fig)
  expect_silent(actual <- wflow_remove(rmd))
  expect_identical(actual$files, c(rmd, fig))
  expect_false(fs::file_exists(rmd))
  expect_false(fs::file_exists(fig))
  expect_false(fs::dir_exists(dir_fig))

  # Same thing, but from docs/ subdirectory
  rmd <- file.path(p$analysis, "new.Rmd")
  fs::file_create(rmd)
  dir_fig <- file.path(p$docs, workflowr:::create_figure_path(rmd))
  fs::dir_create(dir_fig)
  fig <- file.path(dir_fig, "test.png")
  fs::file_create(fig)
  expect_silent(with_dir(p$docs,
                         actual <- wflow_remove(file.path("../analysis", basename(rmd)))))
  expect_false(fs::file_exists(rmd))
  expect_false(fs::file_exists(fig))
  expect_false(fs::dir_exists(dir_fig))
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

test_that("wflow_remove throws an error if user.name and user.email are not set", {

  skip_on_cran()

  # local_no_gitconfig() is defined in tests/testthat/helpers.R
  local_no_gitconfig("-workflowr")

  # Also have to remove local ./.git/config in the project's Git repo. Couldn't
  # figure out a good way to do this with withr. Couldn't get to "restore"
  # function to run at the end of the function call.
  gitconfig <- file.path(tdir, ".git", "config")
  gitconfig_tmp <- file.path(tempdir(), "config")
  file.rename(gitconfig, gitconfig_tmp)
  on.exit(file.rename(gitconfig_tmp, gitconfig), add = TRUE)

  about <- file.path(p$analysis, "about.Rmd")
  expect_error(wflow_remove(about, project = tdir),
               "You must set your user.name and user.email for Git first")
  expect_error(wflow_remove(about, project = tdir),
               "`wflow_remove` with `git = TRUE`")
})
