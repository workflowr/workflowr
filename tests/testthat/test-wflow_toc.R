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
  observed <- wflow_toc(ignore_nav_bar = FALSE, project = path)
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
  observed <- wflow_toc(project = path)
  expect_identical(observed, expected)
})

test_that("wflow_toc ignores HTML files in navigation bar by default", {

  skip_on_cran()

  path <- test_setup()
  on.exit(test_teardown(path))
  s <- wflow_status(project = path)
  wflow_publish(rownames(s$status), view = FALSE, project = path)

  expected <- character()
  observed <- wflow_toc(project = path)
  expect_identical(observed, expected)

  rmd <- file.path(path, "analysis/file.Rmd")
  fs::file_create(rmd)
  wflow_publish(rmd, view = FALSE, project = path)

  expected <- c("1. [file.Rmd](file.html)")
  observed <- wflow_toc(project = path)
  expect_identical(observed, expected)

  expected <- c("1. [About](about.html)",
                "1. [file.Rmd](file.html)",
                "1. [Home](index.html)",
                "1. [License](license.html)")
  observed <- wflow_toc(ignore_nav_bar = FALSE, project = path)
  expect_identical(observed, expected)
})
