context("wflow_git_config")

# Setup ------------------------------------------------------------------------

# Load helper function local_no_gitconfig()
source("helpers.R", local = TRUE)

# Test wflow_git_config --------------------------------------------------------

test_that("wflow_git_config has no effect when run without arguments", {

  skip_on_cran()

  local_no_gitconfig("-workflowr")

  result <- wflow_git_config()
  git_config <- git2r::config(global = TRUE)
  expect_null(git_config$global$user.name)
  expect_null(git_config$global$user.email)
  expect_null(git_config$global$core.editor)
  expect_identical(utils::capture.output(result),
                   c("Current Git user.name needs to be configured",
                     "Current Git user.email needs to be configured",
                     "Other Git settings:", ""))
})

test_that("wflow_git_config can set user.name", {

  skip_on_cran()

  local_no_gitconfig("-workflowr")

  result <- wflow_git_config(user.name = "A Name")
  git_config <- git2r::config(global = TRUE)
  expect_true(git_config$global$user.name == "A Name")
  expect_identical(utils::capture.output(result),
                   c("Current Git user.name:\tA Name",
                     "Current Git user.email needs to be configured",
                     "Other Git settings:", ""))
})

test_that("wflow_git_config can set user.email", {

  skip_on_cran()

  local_no_gitconfig("-workflowr")

  result <- wflow_git_config(user.email = "email@domain")
  git_config <- git2r::config(global = TRUE)
  expect_true(git_config$global$user.email == "email@domain")
  expect_identical(utils::capture.output(result),
                   c("Current Git user.name needs to be configured",
                     "Current Git user.email:\temail@domain",
                     "Other Git settings:", ""))
})

test_that("wflow_git_config can set arbitrary settings", {

  skip_on_cran()

  local_no_gitconfig("-workflowr")

  result <- wflow_git_config(core.editor = "nano")
  git_config <- git2r::config(global = TRUE)
  expect_true(git_config$global$core.editor == "nano")
  expect_identical(utils::capture.output(result),
                   c("Current Git user.name needs to be configured",
                     "Current Git user.email needs to be configured",
                     "Other Git settings:",
                     "\tcore.editor:\tnano", ""))
})
