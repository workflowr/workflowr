context("pkg-options")

# Setup ------------------------------------------------------------------------

source("setup.R")

# Test package options ---------------------------------------------------------

test_that("workflowr does not overwrite user-defined package options", {
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

  sysgit <- callr::r_copycat(function() getOption("workflowr.sysgit"))
  expect_identical(sysgit, "/git")

  view <- callr::r_copycat(function() getOption("workflowr.view"))
  expect_identical(view, "bananas")
})
