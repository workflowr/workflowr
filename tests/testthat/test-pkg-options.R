context("pkg-options")

# Setup ------------------------------------------------------------------------

source("setup.R")

skip_on_cran_windows()

# Test package options ---------------------------------------------------------

test_that("workflowr does not overwrite user-defined package options", {

  sysgit <- callr::r_safe(function() {
    options(workflowr.sysgit = "/git")
    library(workflowr)
    getOption("workflowr.sysgit")
  })

  expect_identical(sysgit, "/git")
})

test_that("workflowr does not overwrite user-defined package options in .Rprofile", {

  if (!interactive()) skip("These tests don't work in R CMD check")
  # Thought this would fix it but it didn't: https://github.com/r-lib/callr/issues/20
  # Interestingly, this worked fine on Windows

  path <- test_setup()
  on.exit(test_teardown(path))

  cwd <- setwd(path)
  on.exit(setwd(cwd), add = TRUE)

  # Have to load workflowr after setting options to properly test this, thus
  # can't append to default .Rprofile that loads workflowr
  writeLines(c("options(workflowr.autosave = FALSE)",
               "options(workflowr.sysgit = \"/git\")",
               "options(workflowr.view = \"bananas\")",
               "library(workflowr)"),
             con = ".Rprofile")

  autosave <- callr::r_safe(function() getOption("workflowr.autosave"),
                            user_profile = TRUE)
  expect_false(autosave)

  sysgit <- callr::r_safe(function() getOption("workflowr.sysgit"),
                          user_profile = TRUE)
  expect_identical(sysgit, "/git")

  view <- callr::r_safe(function() getOption("workflowr.view"),
                        user_profile = TRUE)
  expect_identical(view, "bananas")
})

test_that("Invalid workflowr.autosave does not crash workflowr", {

  path <- test_setup()
  on.exit(test_teardown(path))

  expect_silent(
    callr::r_safe(function(path) {
      options(workflowr.autosave = "not-a-logical")
      library(workflowr)
      wflow_status(project = path)
    }, args = list(path = path))
  )
})
