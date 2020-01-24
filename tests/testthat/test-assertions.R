context("assertions")

# assert_is_flag() -------------------------------------------------------------

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
  expect_error(assert_is_flag(arg_name), "vector with length equal to 1")
})

test_that("assert_is_flag returns original argument name of calling function", {
  path <- fs::file_temp()
  expect_error(wflow_start(path, git = NULL), "git")
  expect_error(wflow_start(path, git = NA), "git")
  expect_error(wflow_start(path, git = 1), "git")
  expect_error(wflow_start(path, git = letters), "git")
  expect_error(wflow_start(path, git = c(TRUE, TRUE)), "git")
})

# assert_is_single_directory() -------------------------------------------------------------

test_that("assert_is_single_directory accepts directory arguments", {
  directory_name <- fs::file_temp()
  fs::dir_create(directory_name)
  expect_silent(assert_is_single_directory(directory_name))
})

test_that("assert_is_single_directory throws error for non-directory arguments", {
  arg_name <- NULL
  expect_error(assert_is_single_directory(arg_name), "arg_name")
  expect_error(assert_is_single_directory(arg_name), "not NULL")
  arg_name <- c(NULL, NULL)
  expect_error(assert_is_single_directory(arg_name), "arg_name")
  expect_error(assert_is_single_directory(arg_name), "not NULL")
  arg_name <- NA
  expect_error(assert_is_single_directory(arg_name), "arg_name")
  expect_error(assert_is_single_directory(arg_name), "not NA")
  arg_name <- c(NA, NA)
  expect_error(assert_is_single_directory(arg_name), "arg_name")
  expect_error(assert_is_single_directory(arg_name), "not NA")
  arg_name <- 1
  expect_error(assert_is_single_directory(arg_name), "arg_name")
  expect_error(assert_is_single_directory(arg_name), "character vector")
  arg_name <- 1:3
  expect_error(assert_is_single_directory(arg_name), "arg_name")
  expect_error(assert_is_single_directory(arg_name), "character vector")
  arg_name <- letters
  expect_error(assert_is_single_directory(arg_name), "arg_name")
  expect_error(assert_is_single_directory(arg_name), "vector with length equal to 1")
  arg_name <- c(FALSE, TRUE)
  expect_error(assert_is_single_directory(arg_name), "arg_name")
  expect_error(assert_is_single_directory(arg_name), "character vector")
  arg_name <- "path/to/non-existent-directory"
  expect_error(assert_is_single_directory(arg_name), "arg_name")
  expect_error(assert_is_single_directory(arg_name), "directory")
})


# assert_has_length() ----------------------------------------------------------

test_that("assert_has_length() accepts valid lengths", {
  expect_silent(assert_has_length(letters[1], 1))
  expect_silent(assert_has_length(letters[1], 1, comparison = "equal to"))
  expect_silent(assert_has_length(letters[1:2], 1, comparison = "greater than"))
  expect_silent(assert_has_length(letters[1], 1, comparison = "greater than or equal to"))
  expect_silent(assert_has_length(letters[1:2], 1, comparison = "greater than or equal to"))
  expect_silent(assert_has_length(character(), 1, comparison = "less than"))
  expect_silent(assert_has_length(letters[1], 5, comparison = "less than"))
  expect_silent(assert_has_length(letters[1], 5, comparison = "less than or equal to"))
  expect_silent(assert_has_length(letters[1:5], 5, comparison = "less than or equal to"))
})

test_that("assert_has_length() throws error for invalid lengths", {
  expect_error(
    assert_has_length(letters[1:2], 1),
    "vector with length equal to 1"
  )
  expect_error(
    assert_has_length(letters[1], 2, comparison = "equal to"),
    "vector with length equal to 2"
  )
  expect_error(
    assert_has_length(letters[1], 2, comparison = "greater than or equal to"),
    "vector with length greater than or equal to 2"
  )
  expect_error(
    assert_has_length(letters[1:2], 2, comparison = "greater than"),
    "vector with length greater than 2"
  )
  expect_error(
    assert_has_length(character(), 1, comparison = "greater than or equal to"),
    "vector with length greater than or equal to 1"
  )
  expect_error(
    assert_has_length(letters[1:5], 5, comparison = "less than"),
    "vector with length less than 5"
  )
  expect_error(
    assert_has_length(letters[1:6], 5, comparison = "less than or equal to"),
    "vector with length less than or equal to 5"
  )
})
