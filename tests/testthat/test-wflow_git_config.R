context("wflow_git_config")

# Setup ------------------------------------------------------------------------

# Temporarily move global .gitconfig file
if (.Platform$OS.type == "windows") {
  # Can't use ~ because the default on Windows is the user's Documents
  # directory.
  # https://cran.r-project.org/bin/windows/base/rw-FAQ.html#What-are-HOME-and-working-directories_003f
  user_home <- workflowr:::get_home()
  config_original <- file.path(user_home, ".gitconfig")
} else {
  config_original <- "~/.gitconfig"
}
if (file.exists(config_original)) {
  config_tmp <- paste0(config_original, "-workflowr")
  file.rename(from = config_original, to = config_tmp)
  on.exit(file.rename(from = config_tmp, to = config_original))
}

# Test wflow_git_config --------------------------------------------------------

test_that("wflow_git_config has no effect when run without arguments", {
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
  result <- wflow_git_config(user.name = "A Name")
  git_config <- git2r::config(global = TRUE)
  expect_true(git_config$global$user.name == "A Name")
  expect_identical(utils::capture.output(result),
                   c("Current Git user.name:\tA Name",
                     "Current Git user.email needs to be configured",
                     "Other Git settings:", ""))
})

test_that("wflow_git_config can set user.email", {
  result <- wflow_git_config(user.email = "email@domain")
  git_config <- git2r::config(global = TRUE)
  expect_true(git_config$global$user.email == "email@domain")
  expect_identical(utils::capture.output(result),
                   c("Current Git user.name:\tA Name",
                     "Current Git user.email:\temail@domain",
                     "Other Git settings:", ""))
})

test_that("wflow_git_config can set arbitrary settings", {
  result <- wflow_git_config(core.editor = "nano")
  git_config <- git2r::config(global = TRUE)
  expect_true(git_config$global$core.editor == "nano")
  expect_identical(utils::capture.output(result),
                   c("Current Git user.name:\tA Name",
                     "Current Git user.email:\temail@domain",
                     "Other Git settings:",
                     "\tcore.editor:\tnano", ""))
})
