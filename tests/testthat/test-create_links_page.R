context("create_links_page")

# Setup ------------------------------------------------------------------------

# Set up a temporary project
site_dir <- base::tempfile("test-links-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE))
site_dir <- workflowr:::absolute(site_dir)
# Cleanup
on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))
# Add analysis files
p <- wflow_paths(project = site_dir)
file.copy(Sys.glob("files/test-create_links_page/[abc].Rmd"), p$analysis)

# Test attempt_date_conversion -------------------------------------------------

test_that("Properly formatted dates are handled correctly", {
  expect_true(attempt_date_conversion("2016-11-01", "example.Rmd") == "2016-11-01")
  expect_true(attempt_date_conversion("2016/11/01", "example.Rmd") == "2016-11-01")
})

test_that("Improperly formatted dates are assigned NA", {
  expect_warning(d <- attempt_date_conversion("Nov 01 2016", "example.Rmd"),
                "Unable to convert the date Nov 01 2016 found in example.Rmd")
  expect_identical(d, NA)
})

# Test create_links_page -------------------------------------------------------

test_that("create_links_page orders files correctly", {

  # Sort by filename (default)
  results_table <- create_links_page(project = site_dir)
  expect_identical(readLines(file.path(p$analysis, "results.Rmd")),
                   readLines("files/test-create_links_page/sort-filename.Rmd"))
  # Sort by title
  results_table <- create_links_page(project = site_dir, sort_method = "title")
  expect_identical(readLines(file.path(p$analysis, "results.Rmd")),
                   readLines("files/test-create_links_page/sort-title.Rmd"))

  # Sort by date, i.e. chronological order
  results_table <- create_links_page(project = site_dir, sort_method = "date")
  expect_identical(readLines(file.path(p$analysis, "results.Rmd")),
                   readLines("files/test-create_links_page/sort-date.Rmd"))

  # Sort by "date reverse", i.e. reverse chronological order
  results_table <- create_links_page(project = site_dir, sort_method = "date reverse")
  expect_identical(readLines(file.path(p$analysis, "results.Rmd")),
                   readLines("files/test-create_links_page/sort-date-reverse.Rmd"))
})

test_that("create_links_page ignores files starting with an underscore", {
  rmd_ignore <- file.path(p$analysis, "_ignore.Rmd")
  file.create(rmd_ignore)
  results_table <- create_links_page(project = site_dir)
  expect_false(basename(rmd_ignore) %in% results_table$rmd)
})
