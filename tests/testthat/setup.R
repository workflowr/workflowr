# Setup ------------------------------------------------------------------------

test_setup <- function(path = tempfile()) {
  suppressMessages(
    wflow_start(path, change_wd = FALSE,
                user.name = "Test Name", user.email = "test@email"))
  path <- workflowr:::absolute(path)
  return(path)
}

test_teardown <- function(path) {
  unlink(path, recursive = TRUE, force = TRUE)
}
