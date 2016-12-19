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
# Remove .gitignore since that is checked separately to test git_init
project_files <- project_files[project_files != ".gitignore"]

git_files <- c(".git", ".gitignore")

test_that("start_project copies files correctly", {

  # start project in a tempdir
  site_dir <- tempfile()
  dir.create(site_dir)
  capture.output(start_project(project_name, site_dir))

  for (f in c(project_files, git_files)) {
    expect_true(file.exists(file.path(site_dir, f)))
  }
  expect_true(file.exists(file.path(site_dir,
                                    paste0(basename(site_dir), ".Rproj"))))
  unlink(site_dir, recursive = TRUE)
})

test_that("start_project git_init = FALSE removes all Git files", {

  # start project in a tempdir
  site_dir <- tempfile()
  dir.create(site_dir)
  capture.output(start_project(project_name, site_dir, git_init = FALSE))

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
