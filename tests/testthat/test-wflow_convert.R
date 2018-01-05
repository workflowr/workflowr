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
# A standard R Markdown files without a yaml header
gold_noyaml <- "files/test-wflow_convert/standard-no-yaml.Rmd"
# An ashlar R Markdown file
gold_ashlar <- "files/test-wflow_convert/ashlar.Rmd"
# A previous workflowr R Markdown file (v0.3.0)
gold_previous <- "files/test-wflow_convert/workflowr-previous.Rmd"

# Test wflow_convert -----------------------------------------------------------

test_that("Can convert standard Rmd to workflowr", {
  tmp_standard <- base::tempfile("standard-", fileext = ".Rmd")
  file.copy(gold_standard, tmp_standard)
  on.exit(unlink(tmp_standard))
  tmp_standard <- workflowr:::absolute(tmp_standard)
  expected <- gold_workflowr_lines
  diffs_list <- wflow_convert(tmp_standard)
  actual <- readLines(tmp_standard)
  expect_identical(actual, expected)
  expect_true(length(diffs_list) == 1 & length(diffs_list[[1]]) > 0)
})

test_that("Can convert standard Rmd to standalone workflowr", {
  tmp_standard <- base::tempfile("standard-", fileext = ".Rmd")
  file.copy(gold_standard, tmp_standard)
  on.exit(unlink(tmp_standard))
  tmp_standard <- workflowr:::absolute(tmp_standard)
  expected <- gold_standalone_lines
  diffs_list <- wflow_convert(tmp_standard, standalone = TRUE)
  actual <- readLines(tmp_standard)
  expect_identical(actual, expected)
  expect_true(length(diffs_list) == 1 & length(diffs_list[[1]]) > 0)
})

test_that("Can add missing yaml header", {
  tmp_noyaml <- base::tempfile("noyaml-", fileext = ".Rmd")
  file.copy(gold_noyaml, tmp_noyaml)
  on.exit(unlink(tmp_noyaml))
  tmp_noyaml <- workflowr:::absolute(tmp_noyaml)
  expected <- gold_workflowr_lines
  diffs_list <- wflow_convert(tmp_noyaml)
  actual <- readLines(tmp_noyaml)
  expect_identical(actual, expected)
  expect_true(length(diffs_list) == 1 & length(diffs_list[[1]]) > 0)
})

test_that("Can convert ashlar file", {
  tmp_ashlar <- base::tempfile("ashlar-", fileext = ".Rmd")
  file.copy(gold_ashlar, tmp_ashlar)
  on.exit(unlink(tmp_ashlar))
  tmp_ashlar <- workflowr:::absolute(tmp_ashlar)
  expected <- gold_workflowr_lines
  diffs_list <- wflow_convert(tmp_ashlar)
  actual <- readLines(tmp_ashlar)
  expect_identical(actual, expected)
  expect_true(length(diffs_list) == 1 & length(diffs_list[[1]]) > 0)
})

test_that("Can convert previous workflowr file", {
  tmp_previous <- base::tempfile("previous-", fileext = ".Rmd")
  file.copy(gold_previous, tmp_previous)
  on.exit(unlink(tmp_previous))
  tmp_previous <- workflowr:::absolute(tmp_previous)
  expected <- gold_workflowr_lines
  diffs_list <- wflow_convert(tmp_previous)
  actual <- readLines(tmp_previous)
  expect_identical(actual, expected)
  expect_true(length(diffs_list) == 1 & length(diffs_list[[1]]) > 0)
})

test_that("Does not affect a current workflowr file if standalone = FALSE", {
  tmp_workflowr <- base::tempfile("workflowr-", fileext = ".Rmd")
  file.copy(gold_workflowr, tmp_workflowr)
  on.exit(unlink(tmp_workflowr))
  tmp_workflowr <- workflowr:::absolute(tmp_workflowr)
  expected <- gold_workflowr_lines
  expect_message(diffs_list <- wflow_convert(tmp_workflowr),
                 "This file is already using the current workflowr format")
  actual <- readLines(tmp_workflowr)
  expect_identical(actual, expected)
  expect_true(length(diffs_list) == 1 & length(diffs_list[[1]]) == 0)
})

test_that("Does not affect a current standalone workflowr file if standalone = TRUE", {
  tmp_standalone <- base::tempfile("standalone-", fileext = ".Rmd")
  file.copy(gold_standalone, tmp_standalone)
  on.exit(unlink(tmp_standalone))
  tmp_standalone <- workflowr:::absolute(tmp_standalone)
  expected <- gold_standalone_lines
  expect_message(diffs_list <- wflow_convert(tmp_standalone, standalone = TRUE),
                 "This file is already using the standalone workflowr format")
  actual <- readLines(tmp_standalone)
  expect_identical(actual, expected)
  expect_true(length(diffs_list) == 1 & length(diffs_list[[1]]) == 0)
})

test_that("Can convert current workflowr file to standalone", {
  tmp_workflowr <- base::tempfile("workflowr-", fileext = ".Rmd")
  file.copy(gold_workflowr, tmp_workflowr)
  on.exit(unlink(tmp_workflowr))
  tmp_workflowr <- workflowr:::absolute(tmp_workflowr)
  expected <- gold_standalone_lines
  diffs_list <- wflow_convert(tmp_workflowr, standalone = TRUE)
  actual <- readLines(tmp_workflowr)
  expect_identical(actual, expected)
  expect_true(length(diffs_list) == 1 & length(diffs_list[[1]]) > 0)
})

test_that("Can convert standalone workflowr file to non-standalone", {
  tmp_standalone <- base::tempfile("standalone-", fileext = ".Rmd")
  file.copy(gold_standalone, tmp_standalone)
  on.exit(unlink(tmp_standalone))
  tmp_standalone <- workflowr:::absolute(tmp_standalone)
  expected <- gold_workflowr_lines
  diffs_list <- wflow_convert(tmp_standalone, standalone = FALSE)
  actual <- readLines(tmp_standalone)
  expect_identical(actual, expected)
  expect_true(length(diffs_list) == 1 & length(diffs_list[[1]]) > 0)
})

test_that("dry_run does not overwrite file and produces diff", {
  tmp_standard <- base::tempfile("standard-", fileext = ".Rmd")
  file.copy(gold_standard, tmp_standard)
  on.exit(unlink(tmp_standard))
  tmp_standard <- workflowr:::absolute(tmp_standard)
  mtime_pre <- file.mtime(tmp_standard)
  Sys.sleep(2)
  expect_message(diffs_list <- wflow_convert(tmp_standard, dry_run = TRUE),
                 "r read-chunk, include=FALSE, cache=FALSE")
  mtime_post <- file.mtime(tmp_standard)
  expect_equal(mtime_post, mtime_pre)
  expect_true(length(diffs_list) == 1 & length(diffs_list[[1]]) > 0)
})

test_that("verbose = FALSE suppresses all output", {
  tmp_standard <- base::tempfile("standard-", fileext = ".Rmd")
  file.copy(gold_standard, tmp_standard)
  on.exit(unlink(tmp_standard))
  tmp_standard <- workflowr:::absolute(tmp_standard)
  expect_silent(diffs_list <- wflow_convert(tmp_standard, dry_run = TRUE,
                                            verbose = FALSE))
  expect_true(length(diffs_list) == 1 & length(diffs_list[[1]]) > 0)
  expect_silent(diffs_list <- wflow_convert(tmp_standard, dry_run = FALSE,
                                            verbose = FALSE))
  expect_true(length(diffs_list) == 1 & length(diffs_list[[1]]) > 0)
})

# Warnings ---------------------------------------------------------------------

test_that("wflow_convert skips missing files and continues processing", {
  tmp_standard <- base::tempfile("standard-", fileext = ".Rmd")
  file.copy(gold_standard, tmp_standard)
  # This one is a relative filepath b/c the test below checks that it matches
  # the output filepath (which is relative).
  tmp_standard <- workflowr:::relative(tmp_standard)
  on.exit(unlink(tmp_standard))
  expected <- gold_workflowr_lines
  expect_warning(success <- wflow_convert(c("missing.Rmd", tmp_standard)),
                 "Skipping missing file: missing.Rmd")
  expect_identical(names(success), tmp_standard)
  actual <- readLines(tmp_standard)
  expect_identical(actual, expected)
})
