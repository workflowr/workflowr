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
  dir.create(site_dir)
  capture.output(wflow_start(project_name, site_dir))

  for (f in c(project_files, git_files)) {
    expect_true(file.exists(file.path(site_dir, f)))
  }
  expect_true(file.exists(file.path(site_dir,
                                    paste0(basename(site_dir), ".Rproj"))))
  unlink(site_dir, recursive = TRUE)
})

test_that("wflow_start git_init = FALSE removes all Git files", {

  # start project in a tempdir
  site_dir <- tempfile()
  dir.create(site_dir)
  capture.output(wflow_start(project_name, site_dir, git_init = FALSE))

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
