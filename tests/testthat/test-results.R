context("results")

test_that("Properly formatted dates are handled correctly", {
  expect_true(attempt_date_conversion("2016-11-01", "example.Rmd") == "2016-11-01")
  expect_true(attempt_date_conversion("2016/11/01", "example.Rmd") == "2016-11-01")
})

test_that("Improperly formatted dates are assigned NA", {
  expect_output(d <- attempt_date_conversion("Nov 01 2016", "example.Rmd"),
                "Unable to convert the date Nov 01 2016 found in example.Rmd")
  expect_identical(d, NA)
})

test_that("create_results orders files correctly", {

  # Set up a temporary project
  site_dir <- tempfile()
  dir.create(site_dir)
  capture.output(create_site("Testing create_results", site_dir))
  # Add analysis files
  file.copy(Sys.glob("files/test_create_results/[abc].Rmd"),
            file.path(site_dir, "analysis"))

  # Sort by filename (default)
  results_table <- create_results(path = site_dir)
  expect_identical(readLines(file.path(site_dir, "analysis/results.Rmd")),
                   readLines("files/test_create_results/sort-filename.Rmd"))
  # Sort by title
  results_table <- create_results(path = site_dir, sort_method = "title")
  expect_identical(readLines(file.path(site_dir, "analysis/results.Rmd")),
                   readLines("files/test_create_results/sort-title.Rmd"))

  # Sort by date, i.e. chronological order
  results_table <- create_results(path = site_dir, sort_method = "date")
  expect_identical(readLines(file.path(site_dir, "analysis/results.Rmd")),
                   readLines("files/test_create_results/sort-date.Rmd"))

  # Sort by "date reverse", i.e. reverse chronological order
  results_table <- create_results(path = site_dir, sort_method = "date reverse")
  expect_identical(readLines(file.path(site_dir, "analysis/results.Rmd")),
                   readLines("files/test_create_results/sort-date-reverse.Rmd"))

  unlink(site_dir, recursive = TRUE)
})
