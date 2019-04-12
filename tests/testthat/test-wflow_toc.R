context("wflow_toc")

source("setup.R")

test_that("wflow_toc returns table of contents", {

  skip_on_cran()

  path <- test_setup()
  on.exit(test_teardown(path))
  s <- wflow_status(project = path)
  wflow_publish(rownames(s$status), view = FALSE, project = path)

  expected <- c("1. [About](about.html)",
                "1. [Home](index.html)",
                "1. [License](license.html)")
  observed <- wflow_toc(ignore_nav_bar = FALSE, clipboard = FALSE, project = path)
  expect_identical(observed, expected)
})

test_that("wflow_toc handles missing titles", {

  skip_on_cran()

  path <- test_setup()
  on.exit(test_teardown(path))
  rmd <- file.path(path, "analysis/no-title.Rmd")
  fs::file_create(rmd)
  wflow_publish(rmd, view = FALSE, project = path)

  expected <- c("1. [no-title.Rmd](no-title.html)")
  observed <- wflow_toc(clipboard = FALSE, project = path)
  expect_identical(observed, expected)
})

test_that("wflow_toc ignores HTML files in navigation bar by default", {

  skip_on_cran()

  path <- test_setup()
  on.exit(test_teardown(path))
  s <- wflow_status(project = path)
  wflow_publish(rownames(s$status), view = FALSE, project = path)

  expected <- character()
  observed <- wflow_toc(clipboard = FALSE, project = path)
  expect_identical(observed, expected)

  rmd <- file.path(path, "analysis/file.Rmd")
  fs::file_create(rmd)
  wflow_publish(rmd, view = FALSE, project = path)

  expected <- c("1. [file.Rmd](file.html)")
  observed <- wflow_toc(clipboard = FALSE, project = path)
  expect_identical(observed, expected)

  expected <- c("1. [About](about.html)",
                "1. [file.Rmd](file.html)",
                "1. [Home](index.html)",
                "1. [License](license.html)")
  observed <- wflow_toc(ignore_nav_bar = FALSE, clipboard = FALSE, project = path)
  expect_identical(observed, expected)
})

test_that("wflow_toc handles missing navbar", {

  skip_on_cran()

  path <- test_setup()
  on.exit(test_teardown(path))
  s <- wflow_status(project = path)
  site_yml <- file.path(s$analysis, "_site.yml")
  yml <- yaml::read_yaml(site_yml)
  yml$navbar <- NULL
  yaml::write_yaml(yml, file = site_yml)
  wflow_publish(rownames(s$status), view = FALSE, project = path)

  expected <- c("1. [About](about.html)",
                "1. [Home](index.html)",
                "1. [License](license.html)")
  observed <- wflow_toc(clipboard = FALSE, project = path)
  expect_identical(observed, expected)
})

test_that("wflow_toc handles navbar on right or both", {

  skip_on_cran()

  path <- test_setup()
  on.exit(test_teardown(path))
  s <- wflow_status(project = path)

  # Both left and right
  site_yml <- file.path(s$analysis, "_site.yml")
  yml <- yaml::read_yaml(site_yml)
  yml$navbar$right <- yml$navbar$left
  yaml::write_yaml(yml, file = site_yml)
  wflow_publish(rownames(s$status), view = FALSE, project = path)

  expected <- character()
  observed <- wflow_toc(clipboard = FALSE, project = path)
  expect_identical(observed, expected)

  # Only on right
  site_yml <- file.path(s$analysis, "_site.yml")
  yml <- yaml::read_yaml(site_yml)
  yml$navbar$left <- NULL
  yaml::write_yaml(yml, file = site_yml)

  expected <- character()
  observed <- wflow_toc(clipboard = FALSE, project = path)
  expect_identical(observed, expected)
})
