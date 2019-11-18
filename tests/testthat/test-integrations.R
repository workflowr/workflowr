context("integrations")

# Integration tests across workflowr functions

# Setup ------------------------------------------------------------------------

source("setup.R")

# Integration tests ------------------------------------------------------------

test_that("Main workflowr functions handle a space in the path", {

  skip_on_cran()

  path <- test_setup(path = fs::file_temp(" a path with spaces "))
  on.exit(test_teardown(path))
  r <- git2r::repository(path)

  # Test wflow_start()
  readme <- file.path(path, "README.md")
  expect_true(fs::file_exists(readme))
  expect_true(readme %in% workflowr:::get_committed_files(r))

  # Test wflow_build()
  suppressMessages(build <- wflow_build(view = FALSE, project = path))
  expect_true(length(build$html) > 0)
  expect_true(all(fs::file_exists(build$html)))
  expect_false(all(build$html %in% workflowr:::get_committed_files(r)))

  # Test wflow_publish()
  index <- file.path(path, "analysis", "index.Rmd")
  suppressMessages(publish <- wflow_publish(index, view = FALSE, project = path))
  expect_identical(workflowr:::absolute(publish$step2$built), index)
  expect_true(workflowr:::absolute(publish$step2$html) %in% workflowr:::get_committed_files(r))

  # Test wflow_status()
  s <- wflow_status(project = path)
  expect_identical(
    workflowr:::absolute(rownames(s$status)[s$status$published]),
    index
  )
})
