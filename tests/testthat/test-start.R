context("start")

# Inspired by rmarkdown tests of render_site
# https://github.com/rstudio/rmarkdown/blob/b95340817f3b285d38be4ba4ceb0a1d280de65f4/tests/testthat/test-site.R

project_name <- "test-project"

infrastructure_path <- system.file("infrastructure/",
                                   package = "workflowr")
project_files <- list.files(path = infrastructure_path, all.files = TRUE,
                            recursive = TRUE)
# Remove Rproj file since that is dynamically renamed
project_files <- project_files[!grepl("Rproj", project_files)]

git_files <- c(".git", ".gitignore")

test_that("wflow_start copies files correctly", {

  # start project in a tempdir
  site_dir <- tempfile()
  capture.output(wflow_start(project_name, site_dir))

  for (f in c(project_files, git_files)) {
    expect_true(file.exists(file.path(site_dir, f)))
  }
  expect_true(file.exists(file.path(site_dir,
                                    paste0(basename(site_dir), ".Rproj"))))
  unlink(site_dir, recursive = TRUE)
})

test_that("wflow_start git = FALSE removes all Git files", {

  # start project in a tempdir
  site_dir <- tempfile()
  capture.output(wflow_start(project_name, site_dir, git = FALSE))

  for (f in project_files) {
    expect_true(file.exists(file.path(site_dir, f)))
  }
  expect_true(file.exists(file.path(site_dir,
                                    paste0(basename(site_dir), ".Rproj"))))
  # Git files do not exist
  for (f in git_files) {
    expect_false(file.exists(file.path(site_dir, f)))
  }
  unlink(site_dir, recursive = TRUE)
})

test_that("wflow_start does not overwrite files by default", {

  # start project in a tempdir
  site_dir <- tempfile()
  dir.create(site_dir)
  readme_file <- file.path(site_dir, "README.md")
  writeLines("original", con = readme_file)
  capture.output(wflow_start(project_name, site_dir))

  readme_contents <- readLines(readme_file)
  expect_true(readme_contents == "original")
  unlink(site_dir, recursive = TRUE)
})

test_that("wflow_start overwrites files when forced", {

  # start project in a tempdir
  site_dir <- tempfile()
  dir.create(site_dir)
  readme_file <- file.path(site_dir, "README.md")
  writeLines("original", con = readme_file)
  capture.output(wflow_start(project_name, site_dir, overwrite = TRUE))

  readme_contents <- readLines(readme_file)
  expect_true(readme_contents[1] == sprintf("# %s", project_name))
  unlink(site_dir, recursive = TRUE)
})

test_that("wflow_start does not overwrite an existing .git directory and does not commit existing files", {

  # start project in a tempdir
  site_dir <- tempfile()
  dir.create(site_dir)
  git2r::init(site_dir)
  r <- git2r::repository(site_dir)
  fake_file <- file.path(site_dir, "file.txt")
  file.create(fake_file)
  git2r::add(r, fake_file)
  git2r::commit(r, message = "The first commit")
  fake_untracked <- file.path(site_dir, "untracked.txt")
  expect_warning(wflow_start(project_name, site_dir),
                 "A .git directory already exists in")
  log <- git2r::commits(r)
  expect_true(length(log) == 2)
  expect_false(fake_untracked %in%
                 workflowr:::obtain_files_in_commit(r, log[[1]]))
  unlink(site_dir, recursive = TRUE)
})

test_that("wflow_start throws an error if user.name and user.email are not set", {
  config_original <- "~/.gitconfig"
  if (file.exists(config_original)) {
    config_tmp <- "~/.gitconfig-workflowr"
    file.rename(from = config_original, to = config_tmp)
    on.exit(file.rename(from = config_tmp, to = config_original))
  }
  site_dir <- tempfile()
  expect_error(wflow_start(project_name, site_dir),
               "You must set your user.name and user.email for Git first\n")
  expect_false(dir.exists(site_dir))
})
