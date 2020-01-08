
assert_is_flag <- function(argument, env = environment()) {
  assert_not_null(argument, env = env)
  assert_not_na(argument, env = env)
  assert_is_logical(argument, env = env)
  assert_has_length(argument, 1, env = env)
}

assert_not_null <- function(argument, env = environment()) {
  if (is_null(argument)) {
    argument_name <- deparse(substitute(argument, env = env))
    stop("Invalid input for argument ", argument_name,
         "\nExpected input: not NULL",
         "\nObserved input: NULL",
         call. = FALSE)
  }
}

assert_not_na <- function(argument, env = environment()) {
  if (is_na(argument)) {
    argument_name <- deparse(substitute(argument, env = env))
    stop("Invalid input for argument ", argument_name,
         "\nExpected input: not NA",
         "\nObserved input: NA",
         call. = FALSE)
  }
}

assert_is_logical <- function(argument, env = environment()) {
  if (!is_logical(argument)) {
    argument_name <- deparse(substitute(argument, env = env))
    stop("Invalid input for argument ", argument_name,
         "\nExpected input: logical vector",
         "\nObserved input: ", deparse(argument),
         call. = FALSE)
  }
}

assert_has_length <- function(argument, required_length, env = environment()) {
  if (!has_length(argument, required_length)) {
    argument_name <- deparse(substitute(argument, env = env))
    stop("Invalid input for argument ", argument_name,
         "\nExpected input: vector with length ", required_length,
         "\nObserved input: vector with length ", length(argument),
         call. = FALSE)
  }
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

has_length <- function(argument, required_length) {
  length(argument) == required_length
}
