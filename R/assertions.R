
assert_is_flag <- function(argument, env = environment()) {
  assert_not_null(argument, env = env)
  assert_not_na(argument, env = env)
  assert_is_logical(argument, env = env)
  assert_has_length(argument, 1, env = env)
}

assert_is_single_directory <- function(argument, env = environment()) {
  assert_not_null(argument, env = env)
  assert_not_na(argument, env = env)
  assert_is_character(argument, env = env)
  assert_has_length(argument, 1, env = env)
  assert_is_directory(argument, env = env)
}

assert_not_null <- function(argument, env = environment()) {
  if (is_null(argument)) {
    argument_name <- deparse(substitute(argument, env = env))
    expected <- "not NULL"
    observed <- "NULL"
    stop_for_assert(argument_name, expected, observed)
  }
}

assert_not_na <- function(argument, env = environment()) {
  if (is_na(argument)) {
    argument_name <- deparse(substitute(argument, env = env))
    expected <- "not NA"
    observed <- "NA"
    stop_for_assert(argument_name, expected, observed)
  }
}

assert_is_logical <- function(argument, env = environment()) {
  if (!is_logical(argument)) {
    argument_name <- deparse(substitute(argument, env = env))
    expected <- "logical vector"
    observed <- deparse(argument)
    stop_for_assert(argument_name, expected, observed)
  }
}

assert_is_character <- function(argument, env = environment()) {
  if (!is_character(argument)) {
    argument_name <- deparse(substitute(argument, env = env))
    expected <- "character vector"
    observed <- deparse(argument)
    stop_for_assert(argument_name, expected, observed)
  }
}

assert_has_length <- function(argument, required_length, env = environment()) {
  if (!has_length(argument, required_length)) {
    argument_name <- deparse(substitute(argument, env = env))
    expected <- paste("vector with length", required_length)
    observed <- paste("vector with length", length(argument))
    stop_for_assert(argument_name, expected, observed)
  }
}

assert_is_directory <- function(argument, env = environment()) {
  if (!is_directory(argument)) {
    argument_name <- deparse(substitute(argument, env = env))
    expected <- "directory"
    observed <- deparse(argument)
    stop_for_assert(argument_name, expected, observed)
  }
}

stop_for_assert <- function(argument_name, expected, observed) {
  stop("Invalid input for argument ", argument_name,
       "\nExpected input: ", expected,
       "\nObserved input: ", observed,
       call. = FALSE)
}

is_null <- function(argument) {
  is.null(argument)
}

is_na <- function(argument) {
  anyNA(argument)
}

is_logical <- function(argument) {
  is.logical(argument)
}

is_character <- function(argument) {
  is.character(argument)
}

is_directory <- function(argument) {
  all(fs::dir_exists(argument))
}

has_length <- function(argument, required_length) {
  length(argument) == required_length
}
