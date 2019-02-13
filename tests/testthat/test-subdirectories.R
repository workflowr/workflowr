context("subdirectories")

# Test that workflowr functions can be run from various subdirectories as long
# as the argument project is set correctly.
#
# Tests the following functions (none that rely on pandoc):
#
# wflow_status, wflow_open, wflow_git_commit, wflow_rename, wflow_rename
#
# in the following directories:
#
# project root, analysis/, docs/, code/, home directory, external directory

source("setup.R")

test_that("workflowr commands can be run from the root of the project", {
  path <- test_setup()
  on.exit(test_teardown(path))
  cwd <- getwd()
  on.exit(setwd(cwd), add = TRUE)
  setwd(path)

  # Create an R Markdown file
  s <- wflow_status()
  rmd1 <- file.path(s$analysis, "test1.Rmd")
  wflow_open(rmd1, change_wd = FALSE, edit_in_rstudio = FALSE)
  expect_true(fs::file_exists(rmd1))
  s <- wflow_status()
  expect_identical(rownames(s$status)[s$status$scratch], rmd1)

  # Commit an R Markdown file
  committed <- wflow_git_commit(rmd1)
  expect_true(rmd1 %in% committed$commit_files)
  s <- wflow_status()
  expect_true(rmd1 %in% rownames(s$status)[s$status$committed])

  # Rename an R Markdown file
  rmd2 <- file.path(s$analysis, "test2.Rmd")
  renamed <- wflow_rename(rmd1, rmd2)
  expect_false(fs::file_exists(rmd1))
  expect_true(rmd1 %in% renamed$files_git)
  expect_true(fs::file_exists(rmd2))
  expect_true(rmd2 %in% renamed$files_git)
  s <- wflow_status()
  expect_true(rmd2 %in% rownames(s$status)[s$status$committed])

  # Remove an R Markdown file
  wflow_remove(rmd2)
  expect_false(fs::file_exists(rmd2))
  s <- wflow_status()
  expect_true(!rmd2 %in% rownames(s$status)[s$status$committed])
})

test_that("workflowr commands can be run from the analysis subdirectory", {
  path <- test_setup()
  on.exit(test_teardown(path))
  cwd <- getwd()
  on.exit(setwd(cwd), add = TRUE)
  setwd(file.path(path, "analysis"))

  # Create an R Markdown file
  s <- wflow_status()
  rmd1 <- "test1.Rmd"
  wflow_open(rmd1, change_wd = FALSE, edit_in_rstudio = FALSE)
  expect_true(fs::file_exists(rmd1))
  s <- wflow_status()
  expect_identical(rownames(s$status)[s$status$scratch], rmd1)

  # Commit an R Markdown file
  committed <- wflow_git_commit(rmd1)
  expect_true(rmd1 %in% committed$commit_files)
  s <- wflow_status()
  expect_true(rmd1 %in% rownames(s$status)[s$status$committed])

  # Rename an R Markdown file
  rmd2 <- "test2.Rmd"
  renamed <- wflow_rename(rmd1, rmd2)
  expect_false(fs::file_exists(rmd1))
  expect_true(rmd1 %in% renamed$files_git)
  expect_true(fs::file_exists(rmd2))
  expect_true(rmd2 %in% renamed$files_git)
  s <- wflow_status()
  expect_true(rmd2 %in% rownames(s$status)[s$status$committed])

  # Remove an R Markdown file
  wflow_remove(rmd2)
  expect_false(fs::file_exists(rmd2))
  s <- wflow_status()
  expect_true(!rmd2 %in% rownames(s$status)[s$status$committed])
})

test_that("workflowr commands can be run from the docs subdirectory", {
  path <- test_setup()
  on.exit(test_teardown(path))
  cwd <- getwd()
  on.exit(setwd(cwd), add = TRUE)
  setwd(file.path(path, "docs"))

  # Create an R Markdown file
  s <- wflow_status()
  rmd1 <- file.path(s$analysis, "test1.Rmd")
  wflow_open(rmd1, change_wd = FALSE, edit_in_rstudio = FALSE)
  expect_true(fs::file_exists(rmd1))
  s <- wflow_status()
  expect_identical(rownames(s$status)[s$status$scratch], rmd1)

  # Commit an R Markdown file
  committed <- wflow_git_commit(rmd1)
  expect_true(rmd1 %in% committed$commit_files)
  s <- wflow_status()
  expect_true(rmd1 %in% rownames(s$status)[s$status$committed])

  # Rename an R Markdown file
  rmd2 <- file.path(s$analysis, "test2.Rmd")
  renamed <- wflow_rename(rmd1, rmd2)
  expect_false(fs::file_exists(rmd1))
  expect_true(rmd1 %in% renamed$files_git)
  expect_true(fs::file_exists(rmd2))
  expect_true(rmd2 %in% renamed$files_git)
  s <- wflow_status()
  expect_true(rmd2 %in% rownames(s$status)[s$status$committed])

  # Remove an R Markdown file
  wflow_remove(rmd2)
  expect_false(fs::file_exists(rmd2))
  s <- wflow_status()
  expect_true(!rmd2 %in% rownames(s$status)[s$status$committed])
})

test_that("workflowr commands can be run from the code subdirectory", {
  path <- test_setup()
  on.exit(test_teardown(path))
  cwd <- getwd()
  on.exit(setwd(cwd), add = TRUE)
  setwd(file.path(path, "code"))

  # Create an R Markdown file
  s <- wflow_status()
  rmd1 <- file.path(s$analysis, "test1.Rmd")
  wflow_open(rmd1, change_wd = FALSE, edit_in_rstudio = FALSE)
  expect_true(fs::file_exists(rmd1))
  s <- wflow_status()
  expect_identical(rownames(s$status)[s$status$scratch], rmd1)

  # Commit an R Markdown file
  committed <- wflow_git_commit(rmd1)
  expect_true(rmd1 %in% committed$commit_files)
  s <- wflow_status()
  expect_true(rmd1 %in% rownames(s$status)[s$status$committed])

  # Rename an R Markdown file
  rmd2 <- file.path(s$analysis, "test2.Rmd")
  renamed <- wflow_rename(rmd1, rmd2)
  expect_false(fs::file_exists(rmd1))
  expect_true(rmd1 %in% renamed$files_git)
  expect_true(fs::file_exists(rmd2))
  expect_true(rmd2 %in% renamed$files_git)
  s <- wflow_status()
  expect_true(rmd2 %in% rownames(s$status)[s$status$committed])

  # Remove an R Markdown file
  wflow_remove(rmd2)
  expect_false(fs::file_exists(rmd2))
  s <- wflow_status()
  expect_true(!rmd2 %in% rownames(s$status)[s$status$committed])
})

test_that("workflowr commands can be run from the home directory", {
  path <- test_setup()
  on.exit(test_teardown(path))
  cwd <- getwd()
  on.exit(setwd(cwd), add = TRUE)
  setwd("~")

  # Create an R Markdown file
  s <- wflow_status(project = path)
  rmd1 <- file.path(s$analysis, "test1.Rmd")
  wflow_open(rmd1, change_wd = FALSE, edit_in_rstudio = FALSE, project = path)
  expect_true(fs::file_exists(rmd1))
  s <- wflow_status(project = path)
  expect_identical(rownames(s$status)[s$status$scratch], rmd1)

  # Commit an R Markdown file
  committed <- wflow_git_commit(rmd1, project = path)
  expect_true(rmd1 %in% committed$commit_files)
  s <- wflow_status(project = path)
  expect_true(rmd1 %in% rownames(s$status)[s$status$committed])

  # Rename an R Markdown file
  rmd2 <- file.path(s$analysis, "test2.Rmd")
  renamed <- wflow_rename(rmd1, rmd2, project = path)
  expect_false(fs::file_exists(rmd1))
  expect_true(rmd1 %in% renamed$files_git)
  expect_true(fs::file_exists(rmd2))
  expect_true(rmd2 %in% renamed$files_git)
  s <- wflow_status(project = path)
  expect_true(rmd2 %in% rownames(s$status)[s$status$committed])

  # Remove an R Markdown file
  wflow_remove(rmd2, project = path)
  expect_false(fs::file_exists(rmd2))
  s <- wflow_status(project = path)
  expect_true(!rmd2 %in% rownames(s$status)[s$status$committed])
})

test_that("workflowr commands can be run from an external directory", {
  path <- test_setup()
  on.exit(test_teardown(path))

  # Create an R Markdown file
  s <- wflow_status(project = path)
  rmd1 <- file.path(s$analysis, "test1.Rmd")
  wflow_open(rmd1, change_wd = FALSE, edit_in_rstudio = FALSE, project = path)
  expect_true(fs::file_exists(rmd1))
  s <- wflow_status(project = path)
  expect_identical(rownames(s$status)[s$status$scratch], rmd1)

  # Commit an R Markdown file
  committed <- wflow_git_commit(rmd1, project = path)
  expect_true(rmd1 %in% committed$commit_files)
  s <- wflow_status(project = path)
  expect_true(rmd1 %in% rownames(s$status)[s$status$committed])

  # Rename an R Markdown file
  rmd2 <- file.path(s$analysis, "test2.Rmd")
  renamed <- wflow_rename(rmd1, rmd2, project = path)
  expect_false(fs::file_exists(rmd1))
  expect_true(rmd1 %in% renamed$files_git)
  expect_true(fs::file_exists(rmd2))
  expect_true(rmd2 %in% renamed$files_git)
  s <- wflow_status(project = path)
  expect_true(rmd2 %in% rownames(s$status)[s$status$committed])

  # Remove an R Markdown file
  wflow_remove(rmd2, project = path)
  expect_false(fs::file_exists(rmd2))
  s <- wflow_status(project = path)
  expect_true(!rmd2 %in% rownames(s$status)[s$status$committed])
})
