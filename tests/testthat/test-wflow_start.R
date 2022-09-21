context("wflow_start")

# Inspired by rmarkdown tests of render_site
# https://github.com/rstudio/rmarkdown/blob/b95340817f3b285d38be4ba4ceb0a1d280de65f4/tests/testthat/test-site.R

# Setup ------------------------------------------------------------------------

source("setup.R")

skip_on_cran_windows()

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
  utils::capture.output(wflow_start(site_dir, change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))
  site_dir <- workflowr:::absolute(site_dir)

  for (f in project_files) {
    expect_true(fs::file_exists(file.path(site_dir, f)))
  }
  expect_true(fs::file_exists(file.path(site_dir,
                                    paste0(basename(site_dir), ".Rproj"))))
  unlink(site_dir, recursive = TRUE, force = TRUE)
})

test_that("wflow_start adds name to analysis/_site.yml and README.md", {

  site_dir <- tempfile()
  utils::capture.output(wflow_start(site_dir, change_wd = FALSE,
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
  utils::capture.output(wflow_start(site_dir, name = project_name, change_wd = FALSE,
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
  utils::capture.output(wflow_start(site_dir, change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))
  site_dir <- workflowr:::absolute(site_dir)

  expect_true(fs::dir_exists(file.path(site_dir, "docs")))
  expect_true(fs::file_exists(file.path(site_dir, "docs", ".nojekyll")))

  unlink(site_dir, recursive = TRUE, force = TRUE)
})

test_that("wflow_start initializes Git repository by default", {

  # start project in a tempdir
  site_dir <- tempfile()
  utils::capture.output(wflow_start(site_dir, change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))
  site_dir <- workflowr:::absolute(site_dir)
  expect_true(git2r::in_repository(site_dir))
  unlink(site_dir, recursive = TRUE, force = TRUE)
})

test_that("wflow_start git = FALSE does not initialize a Git repository", {

  # start project in a tempdir
  site_dir <- tempfile()
  o <- wflow_start(site_dir, git = FALSE, change_wd = FALSE,
                   user.name = "Test Name", user.email = "test@email")
  site_dir <- workflowr:::absolute(site_dir)
  expect_false(git2r::in_repository(site_dir))
  expect_null(o$commit)
  unlink(site_dir, recursive = TRUE, force = TRUE)
})

test_that("wflow_start commits all the project files", {

  # start project in a tempdir
  site_dir <- tempfile()
  utils::capture.output(wflow_start(site_dir, change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))
  site_dir <- workflowr:::absolute(site_dir)

  r <- git2r::repository(site_dir)
  committed <- workflowr:::get_committed_files(r)

  for (f in project_files) {
    expect_true(file.path(site_dir, f) %in% committed)
  }
  # Rproj file
  expect_true(file.path(site_dir, paste0(basename(site_dir), ".Rproj")) %in% committed)
  # hidden files
  expect_true(file.path(site_dir, ".gitattributes") %in% committed)
  expect_true(file.path(site_dir, ".gitignore") %in% committed)
  expect_true(file.path(site_dir, ".Rprofile") %in% committed)
  expect_true(file.path(site_dir, "docs/.nojekyll") %in% committed)
  # note: I'm pretty sure the above tests for hidden files are no longer
  # necessary now that wflow_start() uses the list `templates` instead of actual
  # files. But they are a useful regression check in case I return to a
  # file-based strategy.

  unlink(site_dir, recursive = TRUE, force = TRUE)
})

test_that("wflow_start does not overwrite files by default", {

  # start project in a tempdir
  site_dir <- tempfile()
  fs::dir_create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  readme_file <- file.path(site_dir, "README.md")
  writeLines("original", con = readme_file)
  rprofile_file <- file.path(site_dir, ".Rprofile")
  writeLines("x <- 1", con = rprofile_file)
  utils::capture.output(wflow_start(site_dir, existing = TRUE,
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
  fs::dir_create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  readme_file <- file.path(site_dir, "README.md")
  writeLines("original", con = readme_file)
  rprofile_file <- file.path(site_dir, ".Rprofile")
  writeLines("x <- 1", con = rprofile_file)
  utils::capture.output(wflow_start(site_dir,
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
  fs::dir_create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  git2r::init(site_dir)
  r <- git2r::repository(site_dir)
  git2r::config(r, user.name = "Test Name", user.email = "test@email")
  fake_file <- file.path(site_dir, "file.txt")
  fs::file_create(fake_file)
  workflowr:::git2r_add(r, fake_file)
  git2r::commit(r, message = "The first commit")
  fake_untracked <- file.path(site_dir, "untracked.txt")
  expect_silent(wflow_start(site_dir, existing = TRUE,
                            change_wd = FALSE))
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
               "You must set your user.name and user.email for Git first")
  expect_false(fs::dir_exists(site_dir))
  expect_error(wflow_start(site_dir, change_wd = FALSE),
               "`wflow_start` with `git = TRUE`")
})

test_that("wflow_start can handle relative path to current directory: .", {

  # start project in a tempdir
  site_dir <- tempfile("test-start-")
  fs::dir_create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  cwd <- getwd()
  setwd(site_dir)
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)

  utils::capture.output(wflow_start(".", existing = TRUE, change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))

  expect_true(fs::file_exists(paste0(basename(site_dir), ".Rproj")))
})

test_that("wflow_start can handle relative path to upstream directory: ..", {

  # start project in a tempdir
  site_dir <- tempfile("test-start-")
  site_dir_subdir <- file.path(site_dir, "random-subdir")
  fs::dir_create(site_dir_subdir)
  site_dir <- workflowr:::absolute(site_dir)
  cwd <- getwd()
  setwd(site_dir_subdir)
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)

  utils::capture.output(wflow_start("..", existing = TRUE, change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))

  expect_true(fs::file_exists(file.path("..", paste0(basename(site_dir), ".Rproj"))))
})

test_that("wflow_start can handle relative paths to non-existent directories", {

  # Create and move to a temp directory
  site_dir <- tempfile("test-start-relative-")
  fs::dir_create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  cwd <- getwd()
  setwd(site_dir)
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Use the current working directory to set path to new directory, e.g. specify
  # "./new" instead of "new". There is no advantage to this more verbose option,
  # but it shouldn't break the code.
  utils::capture.output(wflow_start("./new", change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))
  expect_true(fs::file_exists("./new/new.Rproj"))

  # Create and move to an unrelated subdirectory
  fs::dir_create("unrelated")
  setwd("unrelated")

  # Start a new workflowr project in an upstream, non-existent directory
  utils::capture.output(wflow_start("../upstream", change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))
  expect_true(fs::file_exists("../upstream/upstream.Rproj"))
})

test_that("wflow_start can handle deeply nested paths that need to be created", {

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
  utils::capture.output(actual <- wflow_start(dir_test, change_wd = FALSE,
                                       user.name = "Test Name",
                                       user.email = "test@email"))
  expect_identical(actual$directory, expected)
  expect_true(fs::file_exists(file.path(expected, "z.Rproj")))
})

test_that("wflow_start can handle deeply nested paths that need to be created and begin with ./", {

  # Create and move to a temp directory
  site_dir <- tempfile("test-deeply-nested-plus-cwd-")
  fs::dir_create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  cwd <- getwd()
  setwd(site_dir)
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)

  dir_test <- "./a/b/c/x/y/z"
  expected <- file.path(workflowr:::absolute("."),
                        substr(dir_test, 3, nchar(dir_test)))
  utils::capture.output(actual <- wflow_start(dir_test, change_wd = FALSE,
                                       user.name = "Test Name",
                                       user.email = "test@email"))
  expect_identical(actual$directory, expected)
  expect_true(fs::file_exists(file.path(expected, "z.Rproj")))
})

test_that("wflow_start can handle deeply nested paths that need to be created and use relative paths", {

  # Create and move to a temp directory
  site_dir <- tempfile("test-deeply-nested-plus-relative-")
  fs::dir_create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  cwd <- getwd()
  setwd(site_dir)
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Create and move to a nested directory
  dir_unrelated <- "1/2/3/4/5/6"
  fs::dir_create(dir_unrelated)
  setwd(dir_unrelated)

  # Start workflowr project in a highly nested upstream directory
  dir_test <- "../../../../../../a/b/c/x/y/z"
  expected <- file.path(site_dir, "a/b/c/x/y/z")
  utils::capture.output(actual <- wflow_start(dir_test, change_wd = FALSE,
                                       user.name = "Test Name",
                                       user.email = "test@email"))
  expect_identical(actual$directory, expected)
  expect_true(fs::file_exists(file.path(expected, "z.Rproj")))
})

test_that("wflow_start throws error when given a deeply nested path that needs to be created, uses relative paths, and is contained within a Git repository", {

  # Create and move to a temp directory
  site_dir <- tempfile("test-deeply-nested-plus-relative-git-")
  fs::dir_create(site_dir)
  site_dir <- workflowr:::absolute(site_dir)
  cwd <- getwd()
  setwd(site_dir)
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Make this base directory a Git repository
  git2r::init(".")

  # Create and move to a nested directory
  dir_unrelated <- "1/2/3/4/5/6"
  fs::dir_create(dir_unrelated)
  setwd(dir_unrelated)

  # Start workflowr project in a highly nested upstream directory
  dir_test <- "../../../../../../a/b/c/x/y/z"
  # Should throw error and not create directory
  expect_error(wflow_start(dir_test, change_wd = FALSE,
                           user.name = "Test Name", user.email = "test@email"),
               site_dir)
  expect_false(fs::dir_exists(file.path(site_dir, "a/b/c/x/y/z")))
})

test_that("wflow_start changes to workflowr directory by default", {

  # start project in a tempdir
  site_dir <- tempfile("test-start-")
  cwd <- getwd()
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)

  utils::capture.output(wflow_start(site_dir, user.name = "Test Name",
                             user.email = "test@email"))
  site_dir <- workflowr:::absolute(site_dir)

  expect_identical(getwd(), site_dir)
})

test_that("wflow_start fails if `overwrite = TRUE` and `existing = FALSE`", {

  site_dir <- tempfile("test-start-")
  site_dir <- workflowr:::absolute(site_dir)
  expect_error(wflow_start(site_dir, change_wd = FALSE, overwrite = TRUE,
                           user.name = "Test Name", user.email = "test@email"),
               "Cannot overwrite non-existent project.")

})

test_that("wflow_start fails early if directory exists and `existing = FALSE`", {

  site_dir <- tempfile("test-start-")
  fs::dir_create(site_dir)
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
  expect_false(fs::dir_exists(site_dir))

})

test_that("wflow_start creates a pre-push hook when disable_remote = TRUE", {

  if (.Platform$OS.type == "windows")
    skip("disable_remote not available on Windows")

  # start project in a tempdir
  site_dir <- tempfile("test-start-")
  cwd <- getwd()
  on.exit(setwd(cwd))
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)

  wflow_start(site_dir, disable_remote = TRUE,
              user.name = "Test Name", user.email = "test@email")
  site_dir <- workflowr:::absolute(site_dir)

  pre_push_file <- ".git/hooks/pre-push"
  expect_true(fs::file_exists(pre_push_file))
  expect_true(workflowr:::file_is_executable(pre_push_file))

  # Confirm the pre-push hook stops execution
  remote <- wflow_use_github("user", "repo", create_on_github = FALSE)
  expect_error(wflow_git_push(username = "username", password = "password"),
               glue::glue("Execution stopped by {pre_push_file}"))
})

test_that("wflow_start throws error for disable_remote = TRUE on Windows", {

  if (.Platform$OS.type != "windows") skip("Only relevant on Windows")

  # start project in a tempdir
  site_dir <- tempfile("test-start-")
  on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)

  expect_error(wflow_start(site_dir, change_wd = FALSE, disable_remote = TRUE,
                           user.name = "Test Name", user.email = "test@email"),
               "disable_remote is not available on Windows")
})


# Test print.wflow_start -------------------------------------------------------

test_that("print.wflow_start works with change_wd = FALSE", {
  tmp_dir <- workflowr:::absolute(tempfile())
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Dry run
  dry_run <- wflow_start(tmp_dir, change_wd = FALSE, dry_run = TRUE,
                         user.name = "Test Name", user.email = "test@email")
  p_dry_run <- utils::capture.output(dry_run)
  expect_identical(p_dry_run[1], "wflow_start (\"dry run mode\"):")
  expect_identical(p_dry_run[2],
                   paste("- New directory will be created at", tmp_dir))
  expect_identical(p_dry_run[3],
                   sprintf("- Project name will be \"%s\"", basename(tmp_dir)))
  expect_identical(p_dry_run[4],
                   paste("- Working directory will continue to be", getwd()))
  expect_identical(stringr::str_sub(p_dry_run[5], 1, 31),
                   "- Git repo will be initiated at")
  expect_identical(p_dry_run[6], "- Files will be committed with Git")

  # Actual run
  actual_run <- wflow_start(tmp_dir, change_wd = FALSE,
                            user.name = "Test Name", user.email = "test@email")
  # Resolve symlink on macOS
  tmp_dir <- workflowr:::absolute(tmp_dir)
  # Have to use discover argument to get same result with /.git/
  r <- git2r::repository(path = tmp_dir, discover = TRUE)
  p_actual_run <- utils::capture.output(actual_run)
  expect_identical(p_actual_run[1], "wflow_start:")
  expect_identical(p_actual_run[2],
                   paste("- New directory created at", tmp_dir))
  expect_identical(p_actual_run[3],
                   sprintf("- Project name is \"%s\"", basename(tmp_dir)))
  expect_identical(p_actual_run[4],
                   paste("- Working directory continues to be", getwd()))
  expect_identical(p_actual_run[5],
                   paste("- Git repo initiated at", git2r::workdir(r)))
  expect_identical(p_actual_run[6],
                   paste("- Files were committed in version",
                         workflowr:::shorten_sha(git2r::branch_target(git2r::repository_head(r)))))
})

test_that("print.wflow_start works with change_wd = FALSE and git = FALSE", {
  tmp_dir <- workflowr:::absolute(tempfile())
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Dry run
  dry_run <- wflow_start(tmp_dir, change_wd = FALSE, git = FALSE, dry_run = TRUE)
  p_dry_run <- utils::capture.output(dry_run)
  expect_identical(p_dry_run[1], "wflow_start (\"dry run mode\"):")
  expect_identical(p_dry_run[2],
                   paste("- New directory will be created at", tmp_dir))
  expect_identical(p_dry_run[3],
                   sprintf("- Project name will be \"%s\"", basename(tmp_dir)))
  expect_identical(p_dry_run[4],
                   paste("- Working directory will continue to be", getwd()))
  expect_identical(p_dry_run[5],
                   paste("- Git repo will not be initiated at", tmp_dir))
  expect_identical(p_dry_run[6], "- Files will not be committed with Git")

  # Actual run
  actual_run <- wflow_start(tmp_dir, change_wd = FALSE, git = FALSE)
  # Resolve symlink on macOS
  tmp_dir <- workflowr:::absolute(tmp_dir)
  p_actual_run <- utils::capture.output(actual_run)
  expect_identical(p_actual_run[1], "wflow_start:")
  expect_identical(p_actual_run[2],
                   paste("- New directory created at", tmp_dir))
  expect_identical(p_actual_run[3],
                   sprintf("- Project name is \"%s\"", basename(tmp_dir)))
  expect_identical(p_actual_run[4],
                   paste("- Working directory continues to be", getwd()))
  expect_identical(p_actual_run[5], "- No Git repo")
})

test_that("print.wflow_start works with change_wd = FALSE and existing = TRUE", {
  tmp_dir <- tempfile()
  on.exit(unlink(tmp_dir, recursive = TRUE))
  fs::dir_create(tmp_dir)
  tmp_dir <- workflowr:::absolute(tmp_dir)

  # Dry run
  dry_run <- wflow_start(tmp_dir, existing = TRUE, change_wd = FALSE, dry_run = TRUE,
                         user.name = "Test Name", user.email = "test@email")
  p_dry_run <- utils::capture.output(dry_run)
  expect_identical(p_dry_run[1], "wflow_start (\"dry run mode\"):")
  expect_identical(p_dry_run[2],
                   paste("- Files will be added to existing directory", tmp_dir))
  expect_identical(p_dry_run[3],
                   sprintf("- Project name will be \"%s\"", basename(tmp_dir)))
  expect_identical(p_dry_run[4],
                   paste("- Working directory will continue to be", getwd()))
  expect_identical(stringr::str_sub(p_dry_run[5], 1, 31),
                   "- Git repo will be initiated at")
  expect_identical(p_dry_run[6], "- Files will be committed with Git")

  # Actual run
  actual_run <- wflow_start(tmp_dir, existing = TRUE, change_wd = FALSE,
                            user.name = "Test Name", user.email = "test@email")
  # Have to use discover argument to get same result with /.git/
  r <- git2r::repository(path = tmp_dir, discover = TRUE)
  p_actual_run <- utils::capture.output(actual_run)
  expect_identical(p_actual_run[1], "wflow_start:")
  expect_identical(p_actual_run[2],
                   paste("- Files added to existing directory", tmp_dir))
  expect_identical(p_actual_run[3],
                   sprintf("- Project name is \"%s\"", basename(tmp_dir)))
  expect_identical(p_actual_run[4],
                   paste("- Working directory continues to be", getwd()))
  expect_identical(p_actual_run[5],
                   paste("- Git repo initiated at", git2r::workdir(r)))
  expect_identical(p_actual_run[6],
                   paste("- Files were committed in version",
                         workflowr:::shorten_sha(git2r::branch_target(git2r::repository_head(r)))))
})

test_that("print.wflow_start works with existing Git repo", {
  tmp_dir <- tempfile()
  on.exit(unlink(tmp_dir, recursive = TRUE))
  fs::dir_create(tmp_dir)
  tmp_dir <- workflowr:::absolute(tmp_dir)
  repo <- git2r::init(tmp_dir)
  config(repo, user.name = "Test Name", user.email = "test@email")
  f <- file.path(tmp_dir, "file")
  fs::file_create(f)
  workflowr:::git2r_add(repo, f)
  git2r::commit(repo, "initial commit")

  # Dry run
  dry_run <- wflow_start(tmp_dir, existing = TRUE, change_wd = FALSE, dry_run = TRUE)
  p_dry_run <- utils::capture.output(dry_run)
  # Have to use discover argument to get same result with /.git/
  r <- git2r::repository(path = tmp_dir, discover = TRUE)
  expect_identical(p_dry_run[1], "wflow_start (\"dry run mode\"):")
  expect_identical(p_dry_run[2],
                   paste("- Files will be added to existing directory", tmp_dir))
  expect_identical(p_dry_run[3],
                   sprintf("- Project name will be \"%s\"", basename(tmp_dir)))
  expect_identical(p_dry_run[4],
                   paste("- Working directory will continue to be", getwd()))
  expect_identical(p_dry_run[5],
                   paste("- Git repo already present at", git2r::workdir(r)))
  expect_identical(p_dry_run[6], "- Files will be committed with Git")

  # Actual run
  actual_run <- expect_silent(wflow_start(tmp_dir, existing = TRUE, change_wd = FALSE))
  p_actual_run <- utils::capture.output(actual_run)
  expect_identical(p_actual_run[1], "wflow_start:")
  expect_identical(p_actual_run[2],
                   paste("- Files added to existing directory", tmp_dir))
  expect_identical(p_actual_run[3],
                   sprintf("- Project name is \"%s\"", basename(tmp_dir)))
  expect_identical(p_actual_run[4],
                   paste("- Working directory continues to be", getwd()))
  expect_identical(p_actual_run[5],
                   paste("- Git repo already present at", git2r::workdir(r)))
  expect_identical(p_actual_run[6],
                   paste("- Files were committed in version",
                         workflowr:::shorten_sha(git2r::branch_target(git2r::repository_head(r)))))
})

test_that("print.wflow_start works with change_wd = TRUE", {
  tmp_dir <- workflowr:::absolute(tempfile())
  cwd <- getwd()
  on.exit(setwd(cwd))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Dry run
  dry_run <- wflow_start(tmp_dir, dry_run = TRUE,
                         user.name = "Test Name", user.email = "test@email")
  p_dry_run <- utils::capture.output(dry_run)
  expect_identical(p_dry_run[1], "wflow_start (\"dry run mode\"):")
  expect_identical(p_dry_run[2],
                   paste("- New directory will be created at", tmp_dir))
  expect_identical(p_dry_run[3],
                   sprintf("- Project name will be \"%s\"", basename(tmp_dir)))
  expect_identical(p_dry_run[4],
                   paste("- Working directory will be changed to", tmp_dir))
  expect_identical(stringr::str_sub(p_dry_run[5], 1, 31),
                   "- Git repo will be initiated at")
  expect_identical(p_dry_run[6], "- Files will be committed with Git")

  # Actual run
  actual_run <- wflow_start(tmp_dir, user.name = "Test Name", user.email = "test@email")
  # Resolve symlink on macOS
  tmp_dir <- workflowr:::absolute(tmp_dir)
  # Have to use discover argument to get same result with /.git/
  r <- git2r::repository(path = tmp_dir, discover = TRUE)
  p_actual_run <- utils::capture.output(actual_run)
  expect_identical(p_actual_run[1], "wflow_start:")
  expect_identical(p_actual_run[2],
                   paste("- New directory created at", tmp_dir))
  expect_identical(p_actual_run[3],
                   sprintf("- Project name is \"%s\"", basename(tmp_dir)))
  expect_identical(p_actual_run[4],
                   paste("- Working directory changed to", tmp_dir))
  expect_identical(p_actual_run[5],
                   paste("- Git repo initiated at", git2r::workdir(r)))
  expect_identical(p_actual_run[6],
                   paste("- Files were committed in version",
                         workflowr:::shorten_sha(git2r::branch_target(git2r::repository_head(r)))))
})
