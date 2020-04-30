context("wflow_start_rstudio")

# Setup ------------------------------------------------------------------------

source("setup.R")

skip_on_cran_windows()

# Load helper function local_no_gitconfig()
source("helpers.R", local = TRUE)

library("git2r")

project_files <- names(workflowr:::templates)
# Remove Rproj file since that is dynamically renamed
project_files <- project_files[project_files != "Rproj"]

# Test wflow_start_rstudio -------------------------------------------------------------

test_that("wflow_start_studio accepts custom name", {

  project_name <- "A new project"
  site_dir <- tempfile()
  cwd <- getwd()
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)
  utils::capture.output(wflow_start_rstudio(directory = site_dir,
                                            name = project_name,
                                            user.name = "Test Name",
                                            user.email = "test@email"))
  site_dir <- workflowr:::absolute(site_dir)

  readme_contents <- readLines(file.path(site_dir, "README.md"))
  expect_identical(readme_contents[1], paste("#", project_name))

  site_yaml_contents <- readLines(file.path(site_dir, "analysis", "_site.yml"))
  expect_identical(site_yaml_contents[1], sprintf('name: "%s"', project_name))
})



test_that("wflow_start_rstudio initializes Git repository by default", {

  # start project in a tempdir
  site_dir <- tempfile()
  cwd <- getwd()
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)
  utils::capture.output(wflow_start_rstudio(site_dir,
                                            user.name = "Test Name",
                                            user.email = "test@email"))
  site_dir <- workflowr:::absolute(site_dir)
  expect_true(git2r::in_repository(site_dir))
})


test_that("wflow_start_rstudio git = FALSE does not initialize a Git repository", {

  # start project in a tempdir
  site_dir <- tempfile()
  cwd <- getwd()
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)
  o <- wflow_start_rstudio(site_dir,
                           git = FALSE,
                           user.name = "Test Name",
                           user.email = "test@email")
  site_dir <- workflowr:::absolute(site_dir)
  expect_false(git2r::in_repository(site_dir))
  expect_null(o$commit)
})

test_that("wflow_start_rstudio does not overwrite files by default", {

  # start project in a tempdir
  site_dir <- tempfile()
  cwd <- getwd()
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)
  fs::dir_create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  readme_file <- file.path(site_dir, "README.md")
  writeLines("original", con = readme_file)
  rprofile_file <- file.path(site_dir, ".Rprofile")
  writeLines("x <- 1", con = rprofile_file)
  utils::capture.output(wflow_start_rstudio(site_dir,
                                            existing = TRUE,
                                            user.name = "Test Name",
                                            user.email = "test@email"))

  readme_contents <- readLines(readme_file)
  expect_true(readme_contents == "original")
  rprofile_contents <- readLines(rprofile_file)
  expect_true(rprofile_contents == "x <- 1")
})


test_that("wflow_start_rstudio overwrites files when forced", {

  # start project in a tempdir
  site_dir <- tempfile()
  cwd <- getwd()
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)
  fs::dir_create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  readme_file <- file.path(site_dir, "README.md")
  writeLines("original", con = readme_file)
  rprofile_file <- file.path(site_dir, ".Rprofile")
  writeLines("x <- 1", con = rprofile_file)
  utils::capture.output(wflow_start_rstudio(site_dir,
                                            existing = TRUE,
                                            overwrite = TRUE,
                                            user.name = "Test Name",
                                            user.email = "test@email"))

  readme_contents <- readLines(readme_file)
  expect_true(readme_contents[1] == sprintf("# %s", basename(site_dir)))
  rprofile_contents <- readLines(rprofile_file)
  expect_false(any(rprofile_contents == "x <- 1"))
})


test_that("wflow_start_rstudio can handle deeply nested paths that need to be created", {

  # Create and move to a temp directory
  site_dir <- tempfile("test-deeply-nested-")
  fs::dir_create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  cwd <- getwd()
  setwd(site_dir)
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)

  dir_test <- "a/b/c/x/y/z"
  expected <- file.path(workflowr:::absolute("."), dir_test)
  utils::capture.output(actual <- wflow_start_rstudio(dir_test,
                                                      user.name = "Test Name",
                                                      user.email = "test@email"))
  expect_identical(actual$directory, expected)
  expect_true(fs::file_exists(file.path(expected, "z.Rproj")))
})

test_that("wflow_start_rstudio throws an error if user.name and user.email are not set", {

  skip_on_cran()

  # local_no_gitconfig() is defined in tests/testthat/helpers.R
  local_no_gitconfig("-workflowr")

  site_dir <- tempfile()
  cwd <- getwd()
  on.exit(setwd(cwd), add = TRUE)
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)
  expect_error(wflow_start_rstudio(site_dir),
               "You must set your user.name and user.email for Git first")
  expect_false(fs::dir_exists(site_dir))
  expect_error(wflow_start_rstudio(site_dir),
               "the RStudio Project Template")
})
