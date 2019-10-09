context("pkg-options")

# Setup ------------------------------------------------------------------------

source("setup.R")

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
  writeLines(c("options(workflowr.sysgit = \"/git\")",
               "options(workflowr.view = \"bananas\")",
               "library(workflowr)"),
             con = ".Rprofile")

  sysgit <- callr::r_safe(function() getOption("workflowr.sysgit"),
                          user_profile = TRUE)
  expect_identical(sysgit, "/git")

  view <- callr::r_safe(function() getOption("workflowr.view"),
                        user_profile = TRUE)
  expect_identical(view, "bananas")
})
