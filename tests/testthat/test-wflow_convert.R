context("wflow_convert")

# Setup ------------------------------------------------------------------------

# A standard R Markdown file
gold_standard <- "files/test-wflow_convert/standard.Rmd"
gold_standard_lines <- readLines(gold_standard)
# A workflowr R Markdown file
gold_workflowr <- "files/test-wflow_convert/workflowr.Rmd"
gold_workflowr_lines <- readLines(gold_workflowr)
# A standalone workflowr R Markdown file
gold_standalone <- "files/test-wflow_convert/workflowr-standalone.Rmd"
gold_standalone_lines <- readLines(gold_standalone)

# Test wflow_convert -----------------------------------------------------------

test_that("Can convert standard Rmd to workflowr", {
  tmp_standard <- tempfile("standard-", fileext = ".Rmd")
  file.copy(gold_standard, tmp_standard)
  on.exit(unlink(tmp_standard))
  expected <- gold_workflowr_lines
  wflow_convert(tmp_standard)
  actual <- readLines(tmp_standard)
  expect_identical(actual, expected)
})

test_that("Can convert standard Rmd to standalone workflowr", {
  tmp_standard <- tempfile("standard-", fileext = ".Rmd")
  file.copy(gold_standard, tmp_standard)
  on.exit(unlink(tmp_standard))
  expected <- gold_standalone_lines
  wflow_convert(tmp_standard, standalone = TRUE)
  actual <- readLines(tmp_standard)
  expect_identical(actual, expected)
})

test_that("dry_run does not overwrite file and produces diff", {
  tmp_standard <- tempfile("standard-", fileext = ".Rmd")
  file.copy(gold_standard, tmp_standard)
  on.exit(unlink(tmp_standard))
  mtime_pre <- file.mtime(tmp_standard)
  expect_message(wflow_convert(tmp_standard, dry_run = TRUE),
                 "r read-chunk, include=FALSE, cache=FALSE")
  mtime_post <- file.mtime(tmp_standard)
  expect_equal(mtime_post, mtime_pre)
})

test_that("verbose = FALSE suppresses all output", {
  tmp_standard <- tempfile("standard-", fileext = ".Rmd")
  file.copy(gold_standard, tmp_standard)
  on.exit(unlink(tmp_standard))
  expect_silent(wflow_convert(tmp_standard, dry_run = TRUE, verbose = FALSE))
  expect_silent(wflow_convert(tmp_standard, dry_run = FALSE, verbose = FALSE))
})

# Warnings ---------------------------------------------------------------------

test_that("wflow_convert skips missing files and continues processing", {
  tmp_standard <- tempfile("standard-", fileext = ".Rmd")
  file.copy(gold_standard, tmp_standard)
  on.exit(unlink(tmp_standard))
  expected <- gold_workflowr_lines
  expect_warning(success <- wflow_convert(c("missing.Rmd", tmp_standard)),
                 "Skipping missing file: missing.Rmd")
  expect_identical(success, tmp_standard)
  actual <- readLines(tmp_standard)
  expect_identical(actual, expected)
})
