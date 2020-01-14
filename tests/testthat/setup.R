# Setup ------------------------------------------------------------------------

test_setup <- function(path = tempfile()) {
  suppressMessages(
    workflowr:::wflow_start_(path, change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))
  path <- workflowr:::absolute(path)
  return(path)
}

test_teardown <- function(path) {
  unlink(path, recursive = TRUE, force = TRUE)
}

skip_on_cran_windows <- function() {
  on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
  os <- .Platform$OS.type
  on_windows <- os == "windows"
  if (on_cran && on_windows) skip("On CRAN Windows machine")
}
