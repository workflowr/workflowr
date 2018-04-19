context("wflow_start")

# Inspired by rmarkdown tests of render_site
# https://github.com/rstudio/rmarkdown/blob/b95340817f3b285d38be4ba4ceb0a1d280de65f4/tests/testthat/test-site.R

# Setup ------------------------------------------------------------------------

# Load helper function local_no_gitconfig()
source("helpers.R", local = TRUE)

library("git2r")

project_files <- names(workflowr:::templates)
# Remove Rproj file since that is dynamically renamed
project_files <- project_files[project_files != "Rproj"]

# Test wflow_start -------------------------------------------------------------

test_that("wflow_start copies files correctly", {

  # start project in a tempdir
  site_dir <- tempfile()
  capture.output(wflow_start(site_dir, change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))
  site_dir <- workflowr:::absolute(site_dir)

  for (f in project_files) {
    expect_true(file.exists(file.path(site_dir, f)))
  }
  expect_true(file.exists(file.path(site_dir,
                                    paste0(basename(site_dir), ".Rproj"))))
  unlink(site_dir, recursive = TRUE, force = TRUE)
})

test_that("wflow_start adds name to analysis/_site.yml and README.md", {

  site_dir <- tempfile()
  capture.output(wflow_start(site_dir, change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))
  site_dir <- workflowr:::absolute(site_dir)

  readme_contents <- readLines(file.path(site_dir, "README.md"))
  expect_identical(readme_contents[1], paste("#", basename(site_dir)))

  site_yaml_contents <- readLines(file.path(site_dir, "analysis", "_site.yml"))
  expect_identical(site_yaml_contents[1], sprintf('name: "%s"', basename(site_dir)))

  unlink(site_dir, recursive = TRUE, force = TRUE)
})

test_that("wflow_start accepts custom name", {

  project_name <- "A new project"
  site_dir <- tempfile()
  capture.output(wflow_start(site_dir, name = project_name, change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))
  site_dir <- workflowr:::absolute(site_dir)

  readme_contents <- readLines(file.path(site_dir, "README.md"))
  expect_identical(readme_contents[1], paste("#", project_name))

  site_yaml_contents <- readLines(file.path(site_dir, "analysis", "_site.yml"))
  expect_identical(site_yaml_contents[1], sprintf('name: "%s"', project_name))

  unlink(site_dir, recursive = TRUE, force = TRUE)
})

test_that("wflow_start creates docs/ directories and .nojekyll files", {

  # start project in a tempdir
  site_dir <- tempfile()
  capture.output(wflow_start(site_dir, change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))
  site_dir <- workflowr:::absolute(site_dir)

  expect_true(dir.exists(file.path(site_dir, "docs")))
  expect_true(file.exists(file.path(site_dir, "docs", ".nojekyll")))
  expect_true(file.exists(file.path(site_dir, "analysis", ".nojekyll")))

  unlink(site_dir, recursive = TRUE, force = TRUE)
})

test_that("wflow_start initializes Git repository by default", {

  # start project in a tempdir
  site_dir <- tempfile()
  capture.output(wflow_start(site_dir, change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))
  site_dir <- workflowr:::absolute(site_dir)
  expect_true(git2r::in_repository(site_dir))
  unlink(site_dir, recursive = TRUE, force = TRUE)
})

test_that("wflow_start git = FALSE does not initialize a Git repository", {

  # start project in a tempdir
  site_dir <- tempfile()
  capture.output(wflow_start(site_dir, git = FALSE, change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))
  site_dir <- workflowr:::absolute(site_dir)
  expect_false(git2r::in_repository(site_dir))
  unlink(site_dir, recursive = TRUE, force = TRUE)
})

test_that("wflow_start commits all the project files", {

  # start project in a tempdir
  site_dir <- tempfile()
  capture.output(wflow_start(site_dir, change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))
  site_dir <- workflowr:::absolute(site_dir)

  r <- git2r::repository(site_dir)
  committed <- workflowr:::get_committed_files(r)

  for (f in project_files) {
    expect_true(f %in% committed)
  }
  # Rproj file
  expect_true(paste0(basename(site_dir), ".Rproj") %in% committed)
  # hidden files
  expect_true(".gitignore" %in% committed)
  expect_true(".Rprofile" %in% committed)
  expect_true("analysis/.nojekyll" %in% committed)
  expect_true("docs/.nojekyll" %in% committed)

  unlink(site_dir, recursive = TRUE, force = TRUE)
})

test_that("wflow_start does not overwrite files by default", {

  # start project in a tempdir
  site_dir <- tempfile()
  dir.create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  readme_file <- file.path(site_dir, "README.md")
  writeLines("original", con = readme_file)
  rprofile_file <- file.path(site_dir, ".Rprofile")
  writeLines("x <- 1", con = rprofile_file)
  capture.output(wflow_start(site_dir, existing = TRUE,
                             change_wd = FALSE, user.name = "Test Name",
                             user.email = "test@email"))

  readme_contents <- readLines(readme_file)
  expect_true(readme_contents == "original")
  rprofile_contents <- readLines(rprofile_file)
  expect_true(rprofile_contents == "x <- 1")
  unlink(site_dir, recursive = TRUE, force = TRUE)
})

test_that("wflow_start overwrites files when forced", {

  # start project in a tempdir
  site_dir <- tempfile()
  dir.create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  readme_file <- file.path(site_dir, "README.md")
  writeLines("original", con = readme_file)
  rprofile_file <- file.path(site_dir, ".Rprofile")
  writeLines("x <- 1", con = rprofile_file)
  capture.output(wflow_start(site_dir,
                             existing = TRUE, overwrite = TRUE,
                             change_wd = FALSE, user.name = "Test Name",
                             user.email = "test@email"))

  readme_contents <- readLines(readme_file)
  expect_true(readme_contents[1] == sprintf("# %s", basename(site_dir)))
  rprofile_contents <- readLines(rprofile_file)
  expect_false(any(rprofile_contents == "x <- 1"))
  unlink(site_dir, recursive = TRUE, force = TRUE)
})

test_that("wflow_start does not overwrite an existing .git directory and does not commit existing files", {

  # start project in a tempdir
  site_dir <- tempfile()
  dir.create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  git2r::init(site_dir)
  r <- git2r::repository(site_dir)
  git2r::config(r, user.name = "Test Name", user.email = "test@email")
  fake_file <- file.path(site_dir, "file.txt")
  file.create(fake_file)
  git2r::add(r, fake_file)
  git2r::commit(r, message = "The first commit")
  fake_untracked <- file.path(site_dir, "untracked.txt")
  expect_warning(wflow_start(site_dir, existing = TRUE,
                             change_wd = FALSE),
                 "A .git directory already exists in")
  log <- git2r::commits(r)
  expect_true(length(log) == 2)
  expect_false(fake_untracked %in%
                 workflowr:::obtain_files_in_commit(r, log[[1]]))
  unlink(site_dir, recursive = TRUE, force = TRUE)
})

test_that("wflow_start throws an error if user.name and user.email are not set", {

  skip_on_cran()

  # local_no_gitconfig() is defined in tests/testthat/helpers.R
  local_no_gitconfig("-workflowr")

  site_dir <- tempfile()
  expect_error(wflow_start(site_dir, change_wd = FALSE),
               "You must set your user.name and user.email for Git first\n")
  expect_false(dir.exists(site_dir))
})

test_that("wflow_start can handle relative path to current directory: .", {

  # start project in a tempdir
  site_dir <- tempfile("test-start-")
  dir.create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  cwd <- getwd()
  setwd(site_dir)
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)

  capture.output(wflow_start(".", existing = TRUE, change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))

  expect_true(file.exists(paste0(basename(site_dir), ".Rproj")))
})

test_that("wflow_start can handle relative path to upstream directory: ..", {

  # start project in a tempdir
  site_dir <- tempfile("test-start-")
  site_dir_subdir <- file.path(site_dir, "random-subdir")
  dir.create(site_dir_subdir, recursive = TRUE)
  site_dir <- workflowr:::absolute(site_dir)
  cwd <- getwd()
  setwd(site_dir_subdir)
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)

  capture.output(wflow_start("..", existing = TRUE, change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))

  expect_true(file.exists(file.path("..", paste0(basename(site_dir), ".Rproj"))))
})

test_that("wflow_start can handle relative paths to non-existent directories", {

  # Create and move to a temp directory
  site_dir <- tempfile("test-start-relative-")
  dir.create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  cwd <- getwd()
  setwd(site_dir)
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Use the current working directory to set path to new directory, e.g. specify
  # "./new" instead of "new". There is no advantage to this more verbose option,
  # but it shouldn't break the code.
  capture.output(wflow_start("./new", change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))
  expect_true(file.exists("./new/new.Rproj"))

  # Create and move to an unrelated subdirectory
  dir.create("unrelated")
  setwd("unrelated")

  # Start a new workflowr project in an upstream, non-existent directory
  capture.output(wflow_start("../upstream", change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))
  expect_true(file.exists("../upstream/upstream.Rproj"))
})


test_that("wflow_start can handle deeply nested paths that need to be created", {

  # Create and move to a temp directory
  site_dir <- tempfile("test-deeply-nested-")
  dir.create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  cwd <- getwd()
  setwd(site_dir)
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)

  dir_test <- "a/b/c/x/y/z"
  expected <- file.path(workflowr:::absolute("."), dir_test)
  capture.output(actual <- wflow_start(dir_test, change_wd = FALSE,
                                       user.name = "Test Name",
                                       user.email = "test@email"))
  expect_identical(actual, expected)
  expect_true(file.exists(file.path(expected, "z.Rproj")))
})

test_that("wflow_start can handle deeply nested paths that need to be created and begin with ./", {

  # Create and move to a temp directory
  site_dir <- tempfile("test-deeply-nested-plus-cwd-")
  dir.create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  cwd <- getwd()
  setwd(site_dir)
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)

  dir_test <- "./a/b/c/x/y/z"
  expected <- file.path(workflowr:::absolute("."),
                        substr(dir_test, 3, nchar(dir_test)))
  capture.output(actual <- wflow_start(dir_test, change_wd = FALSE,
                                       user.name = "Test Name",
                                       user.email = "test@email"))
  expect_identical(actual, expected)
  expect_true(file.exists(file.path(expected, "z.Rproj")))
})

test_that("wflow_start can handle deeply nested paths that need to be created and use relative paths", {

  # Create and move to a temp directory
  site_dir <- tempfile("test-deeply-nested-plus-relative-")
  dir.create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  cwd <- getwd()
  setwd(site_dir)
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Create and move to a nested directory
  dir_unrelated <- "1/2/3/4/5/6"
  dir.create(dir_unrelated, recursive = TRUE)
  setwd(dir_unrelated)

  # Start workflowr project in a highly nested upstream directory
  dir_test <- "../../../../../../a/b/c/x/y/z"
  expected <- file.path(site_dir, "a/b/c/x/y/z")
  capture.output(actual <- wflow_start(dir_test, change_wd = FALSE,
                                       user.name = "Test Name",
                                       user.email = "test@email"))
  expect_identical(actual, expected)
  expect_true(file.exists(file.path(expected, "z.Rproj")))
})

test_that("wflow_start throws error when given a deeply nested path that needs to be created, uses relative paths, and is contained within a Git repository", {

  # Create and move to a temp directory
  site_dir <- tempfile("test-deeply-nested-plus-relative-git-")
  dir.create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  cwd <- getwd()
  setwd(site_dir)
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Make this base directory a Git repository
  git2r::init(".")

  # Create and move to a nested directory
  dir_unrelated <- "1/2/3/4/5/6"
  dir.create(dir_unrelated, recursive = TRUE)
  setwd(dir_unrelated)

  # Start workflowr project in a highly nested upstream directory
  dir_test <- "../../../../../../a/b/c/x/y/z"
  # Should throw error and not create directory
  expect_error(wflow_start(dir_test, change_wd = FALSE,
                           user.name = "Test Name", user.email = "test@email"),
               site_dir)
  expect_false(dir.exists(file.path(site_dir, "a/b/c/x/y/z")))
})

test_that("wflow_start changes to workflowr directory by default", {

  # start project in a tempdir
  site_dir <- tempfile("test-start-")
  cwd <- getwd()
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)

  capture.output(wflow_start(site_dir, user.name = "Test Name",
                             user.email = "test@email"))
  site_dir <- workflowr:::absolute(site_dir)

  expect_identical(getwd(), site_dir)
})

test_that("wflow_start fails early if directory exists and `existing = FALSE`", {

  site_dir <- tempfile("test-start-")
  dir.create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))

  expect_error(wflow_start(site_dir, change_wd = FALSE,
                           user.name = "Test Name", user.email = "test@email"),
               "Directory already exists. Set existing = TRUE if you wish to add workflowr files to an already existing project.")

})

test_that("wflow_start fails early if directory does not exist and `existing = TRUE`", {

  site_dir <- tempfile("test-start-")

  expect_error(wflow_start(site_dir, existing = TRUE, change_wd = FALSE,
                           user.name = "Test Name", user.email = "test@email"),
               "Directory does not exist. Set existing = FALSE to create a new directory for the workflowr files.")
  expect_false(dir.exists(site_dir))

})
