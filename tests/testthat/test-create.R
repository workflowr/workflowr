context("create")

# Inspired by rmarkdown tests of render_site
# https://github.com/rstudio/rmarkdown/blob/b95340817f3b285d38be4ba4ceb0a1d280de65f4/tests/testthat/test-site.R

project_name <- "test-project"

project_files <- c("analysis/about.Rmd",
                   "analysis/chunks/_sessioninfo.Rmd",
                   "analysis/chunks/_setup.Rmd",
                   "analysis/include/footer.html",
                   "analysis/index.Rmd",
                   "analysis/.nojekyll",
                   "analysis/README.md",
                   "analysis/_site.yml",
                   "code/README.md",
                   "code/script.py",
                   "code/script.R",
                   "code/script.sh",
                   "data/README.md",
                   "output/README.md")

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
