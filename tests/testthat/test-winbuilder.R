context("winbuilder")

# Attempting to diagnose winbuilder-specific errors

# if (!(.Platform$OS.type == "windows" && Sys.getenv("USERPROFILE") == "C:\\Users\\CRAN"))
#   skip("Only relevant on winbuilder")

test_that("git2r can add and commit a file", {

  if (packageVersion("git2r") <= as.numeric_version("0.21.0"))
    skip("requires S3 version of git2r")

  # Adapting the example from ?git2r::commit
  path <- tempfile(pattern = "git2r-")
  dir.create(path)
  path <- workflowr:::absolute(path)
  repo <- git2r::init(path)
  git2r::config(repo, user.name = "Alice", user.email = "alice@example.org")
  writeLines("Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do",
             file.path(path, "example.txt"))
  git2r::add(repo, "example.txt")
  commit_1 <- git2r::commit(repo, "First commit message")
  expect_identical(class(commit_1), "git_commit")
})

test_that("git2r can add and commit a file using an absolute path", {

  if (packageVersion("git2r") <= as.numeric_version("0.21.0"))
    skip("requires S3 version of git2r")

  # Adapting the example from ?git2r::commit
  path <- tempfile(pattern = "git2r-")
  dir.create(path)
  path <- workflowr:::absolute(path)
  repo <- git2r::init(path)
  git2r::config(repo, user.name = "Alice", user.email = "alice@example.org")
  writeLines("Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do",
             file.path(path, "example.txt"))
  git2r::add(repo, file.path(path, "example.txt"))
  commit_1 <- git2r::commit(repo, "First commit message")
  expect_identical(class(commit_1), "git_commit")
})

test_that("wflow_git_commit can add and commit a file using an absolute path", {

  if (packageVersion("git2r") <= as.numeric_version("0.21.0"))
    skip("requires S3 version of git2r")

  site_dir <- tempfile("test-wflow_git_commit-")
  suppressMessages(wflow_start(site_dir, change_wd = FALSE, user.name = "Test Name",
                               user.email = "test@email"))
  site_dir <- workflowr:::absolute(site_dir)
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))
  r <- git2r::repository(path = site_dir)
  s <- wflow_status(project = site_dir)

  writeLines("Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do",
             file.path(site_dir, "example.txt"))
  git2r::add(r, "example.txt")
  commit_1 <- git2r::commit(r, "First commit message")
  expect_identical(class(commit_1), "git_commit")

  writeLines("overwrite",
             file.path(site_dir, "example.txt"))
  o <- wflow_git_commit(file.path(site_dir, "example.txt"), project = site_dir)

  commit_2 <- o$commit
  expect_identical(class(commit_1), "git_commit")
})

test_that("wflow_git_commit can add and commit a file using a relative path", {

  if (packageVersion("git2r") <= as.numeric_version("0.21.0"))
    skip("requires S3 version of git2r")

  site_dir <- tempfile("test-wflow_git_commit-")
  suppressMessages(wflow_start(site_dir, change_wd = FALSE, user.name = "Test Name",
                               user.email = "test@email"))
  site_dir <- workflowr:::relative(site_dir)
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))
  r <- git2r::repository(path = site_dir)
  s <- wflow_status(project = site_dir)

  writeLines("Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do",
             file.path(site_dir, "example.txt"))
  git2r::add(r, "example.txt")
  commit_1 <- git2r::commit(r, "First commit message")
  expect_identical(class(commit_1), "git_commit")

  writeLines("overwrite",
             file.path(site_dir, "example.txt"))
  o <- wflow_git_commit(file.path(site_dir, "example.txt"), project = site_dir)

  commit_2 <- o$commit
  expect_identical(class(commit_1), "git_commit")
})

test_that("Test fs::dir_exists with absolute paths", {
  f <- fs::file_temp()
  expect_identical(fs::path_abs(fs::path_rel(f)), f)
  fs::dir_create(f)
  expect_true(fs::dir_exists(f))
  expect_true(fs::dir_exists(paste0(f, "/")))
  expect_true(fs::dir_exists(paste0(f, "\\")))
  fs::dir_delete(f)
})

test_that("Test fs::dir_exists with relative paths", {
  f <- "testing"
  expect_identical(fs::path_abs(fs::path_rel(f)), fs::path_abs(f))
  fs::dir_create(f)
  expect_true(fs::dir_exists(f))
  expect_true(fs::dir_exists(paste0(f, "/")))
  expect_true(fs::dir_exists(paste0(f, "\\")))
  fs::dir_delete(f)
})

test_that("workflowr can interchange absolute/relative paths on winbuilder", {
  f <- tempfile()
  expect_identical(workflowr:::absolute(workflowr:::relative(f)),
                   workflowr:::absolute(f))
  file.create(f)
  expect_true(file.exists(f))
  expect_true(file.exists(workflowr:::absolute(workflowr:::relative(f))))
})
