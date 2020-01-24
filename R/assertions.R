# Process the files passed as input to workflowr functions.
#
# allow_null - Allow passing files=NULL
# files_only - Throw an error if a path to a directory is included
# rmd_only - Only allow files extensions Rmd or rmd
# must_exist - Paths must exist on filesystem
# convert_to_relative_paths - Return paths relative to working directory
# expland_glob - Pass through function glob()
process_input_files <- function(files,
                                allow_null = FALSE,
                                files_only = TRUE,
                                rmd_only = FALSE,
                                must_exist = TRUE,
                                convert_to_relative_paths = FALSE,
                                expand_glob = TRUE) {

  if (allow_null && is_null(files)) {
    return(NULL)
  }

  assert_not_null(files)
  assert_is_character(files)
  assert_has_length(files, required_length = 1, comparison = "greater than or equal to")

  if (files_only) {
    if (any(fs::dir_exists(files))) {
      stop("files cannot include a path to a directory")
    }
  }

  if (expand_glob) {
    files <- glob(files)
  }

  if (must_exist) {
    if (!all(fs::file_exists(files)))
      stop("Not all files exist. Check the paths to the files")
  }

  if (convert_to_relative_paths) {
    files <- relative(files)
  }

  if (rmd_only) {
    assert_is_rmd(files)
  }

  return(files)
}

assert_is_rmd <- function(argument, env = environment()) {
  if (!is_rmd(argument)) {
    argument_name <- deparse(substitute(argument, env = env))
    expected <- "Only files with extension Rmd or rmd"
    observed <- deparse(argument)
    stop_for_assert(argument_name, expected, observed)
  }
}

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

assert_has_length <- function(argument, required_length,
                              comparison = c("equal to",
                                             "greater than",
                                             "greater than or equal to",
                                             "less than",
                                             "less than or equal to"),
                              env = environment()) {
  comparison <- match.arg(comparison)
  if (!has_length(argument, required_length, comparison)) {
    argument_name <- deparse(substitute(argument, env = env))
    expected <- paste("vector with length", comparison, required_length)
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

is_rmd <- function(argument) {
  extensions <- fs::path_ext(argument)
  all(stringr::str_detect(extensions, "^[Rr]md$"))
}

has_length <- function(argument, required_length, comparison) {
  switch(comparison,
         `equal to` = length(argument) == required_length,
         `greater than` = length(argument) > required_length,
         `greater than or equal to` = length(argument) >= required_length,
         `less than` = length(argument) < required_length,
         `less than or equal to` = length(argument) <= required_length)
}
