context("create")

# Inspired by rmarkdown tests of render_site
# https://github.com/rstudio/rmarkdown/blob/b95340817f3b285d38be4ba4ceb0a1d280de65f4/tests/testthat/test-site.R

project_name <- "test-project"

infrastructure_path <- system.file("infrastructure/",
                                   package = "workflowr")
project_files <- list.files(path = infrastructure_path, all.files = TRUE,
                            recursive = TRUE)
# Remove Rproj file since that is dynamically renamed
project_files <- project_files[!grepl("Rproj", project_files)]
# Remove .gitignore since that is checked separately to test git_init
project_files <- project_files[project_files != ".gitignore"]

git_files <- c(".git", ".gitignore")

test_that("create_site copies files correctly", {

  # create site in a tempdir
  site_dir <- tempfile()
  dir.create(site_dir)
  capture.output(create_site(project_name, site_dir))

  for (f in c(project_files, git_files)) {
    expect_true(file.exists(file.path(site_dir, f)))
  }
  expect_true(file.exists(file.path(site_dir,
                                    paste0(basename(site_dir), ".Rproj"))))
})

test_that("create_site git_init = FALSE removes all Git files", {

  # create site in a tempdir
  site_dir <- tempfile()
  dir.create(site_dir)
  capture.output(create_site(project_name, site_dir, git_init = FALSE))

  for (f in project_files) {
    expect_true(file.exists(file.path(site_dir, f)))
  }
  expect_true(file.exists(file.path(site_dir,
                                    paste0(basename(site_dir), ".Rproj"))))
  # Git files do not exist
  for (f in git_files) {
    expect_false(file.exists(file.path(site_dir, f)))
  }
})

test_that("create_site rstudio = FALSE removes RStudio Project file", {

  # create site in a tempdir
  site_dir <- tempfile()
  dir.create(site_dir)
  capture.output(create_site(project_name, site_dir, rstudio = FALSE))

  for (f in c(project_files, git_files)) {
    expect_true(file.exists(file.path(site_dir, f)))
  }
  # RStudio Project file does not exist
  expect_false(file.exists(file.path(site_dir,
                                    paste0(basename(site_dir), ".Rproj"))))
})
