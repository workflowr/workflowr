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

test_that("wflow_git_config only overwrites existing settings when requested", {

  skip_on_cran()

  local_no_gitconfig("-workflowr")

  # Set some Git config settings
  git2r::config(global = TRUE,
                user.name = "original name",
                user.email = "original email",
                core.editor = "vi",
                color.ui = "never")

  expect_error(
    wflow_git_config(user.name = "new name"),
    "Some settings already exist"
  )
  expect_error(
    wflow_git_config(user.email = "new email"),
    "Some settings already exist"
  )
  expect_error(
    wflow_git_config(core.editor = "nano"),
    "Some settings already exist"
  )
  expect_error(
    wflow_git_config(color.ui = "always"),
    "Some settings already exist"
  )

  config_latest <- git2r::config(global = TRUE)$global
  expect_identical(config_latest$user.name, "original name")
  expect_identical(config_latest$user.email, "original email")
  expect_identical(config_latest$core.editor, "vi")
  expect_identical(config_latest$color.ui, "never")

  expect_message(
    wflow_git_config(user.name = "new name",
                     user.email = "new email",
                     core.editor = "nano",
                     color.ui = "always",
                     overwrite = TRUE),
    "The settings above will be overwritten."
  )

  config_latest <- git2r::config(global = TRUE)$global
  expect_identical(config_latest$user.name, "new name")
  expect_identical(config_latest$user.email, "new email")
  expect_identical(config_latest$core.editor, "nano")
  expect_identical(config_latest$color.ui, "always")
})
