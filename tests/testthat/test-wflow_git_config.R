context("wflow_git_config")

# Setup ------------------------------------------------------------------------

# Functions to temporarily move global .gitconfig file

library("withr")

# withr "set" function
#
# Moves .gitconfig to .gitconfig-suffix
remove_gitconfig <- function(suffix) {
  user_home <- workflowr:::get_home()
  config_original <- file.path(user_home, ".gitconfig")
  config_tmp <- paste0(config_original, suffix)
  if (file.exists(config_original)) {
    file.rename(from = config_original, to = config_tmp)
  }
  return(config_tmp)
}

# withr "reset" function
#
# Moves .gitconfig-suffix to .gitconfig
restore_gitconfig <- function(config_tmp) {
  user_home <- workflowr:::get_home()
  config_original <- file.path(user_home, ".gitconfig")
  if (file.exists(config_tmp)) {
    file.rename(from = config_tmp, to = config_original)
  }
}

local_no_gitconfig <- withr::local_(set = remove_gitconfig,
                                    reset = restore_gitconfig)

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
