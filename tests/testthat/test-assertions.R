context("assertions")

test_that("assert_is_flag accepts flag arguments", {
  arg_name <- TRUE
  expect_silent(assert_is_flag(arg_name))
  arg_name <- FALSE
  expect_silent(assert_is_flag(arg_name))
})

test_that("assert_is_flag throws error for non-flag arguments", {
  arg_name <- NULL
  expect_error(assert_is_flag(arg_name), "arg_name")
  expect_error(assert_is_flag(arg_name), "not NULL")
  arg_name <- c(NULL, NULL)
  expect_error(assert_is_flag(arg_name), "arg_name")
  expect_error(assert_is_flag(arg_name), "not NULL")
  arg_name <- NA
  expect_error(assert_is_flag(arg_name), "arg_name")
  expect_error(assert_is_flag(arg_name), "not NA")
  arg_name <- c(NA, NA)
  expect_error(assert_is_flag(arg_name), "arg_name")
  expect_error(assert_is_flag(arg_name), "not NA")
  arg_name <- 1
  expect_error(assert_is_flag(arg_name), "arg_name")
  expect_error(assert_is_flag(arg_name), "logical vector")
  arg_name <- 1:3
  expect_error(assert_is_flag(arg_name), "arg_name")
  expect_error(assert_is_flag(arg_name), "logical vector")
  arg_name <- letters
  expect_error(assert_is_flag(arg_name), "arg_name")
  expect_error(assert_is_flag(arg_name), "logical vector")
  arg_name <- c(FALSE, TRUE)
  expect_error(assert_is_flag(arg_name), "arg_name")
  expect_error(assert_is_flag(arg_name), "vector with length 1")
})

test_that("assert_is_flag returns original argument name of calling function", {
  path <- fs::file_temp()
  expect_error(wflow_start(path, git = NULL), "git")
  expect_error(wflow_start(path, git = NA), "git")
  expect_error(wflow_start(path, git = 1), "git")
  expect_error(wflow_start(path, git = letters), "git")
  expect_error(wflow_start(path, git = c(TRUE, TRUE)), "git")
})
