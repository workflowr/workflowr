context("wflow_toc")

source("setup.R")

test_that("multiplication works", {

  skip_on_cran()

  path <- test_setup()
  on.exit(test_teardown(path))
  s <- wflow_status(project = path)
  wflow_publish(rownames(s$status), view = FALSE, project = path)

  expected <- c("1. [About](about.html)",
                "1. [Home](index.html)",
                "1. [License](license.html)")
  observed <- wflow_toc(project = path)
  expect_identical(observed, expected)
})
