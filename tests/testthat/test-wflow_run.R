context("wflow_run")

# Setup ------------------------------------------------------------------------

source("setup.R")

skip_on_cran_windows()

# Test wflow_run ---------------------------------------------------------------

test_that("wflow_run by default runs the most recently modified file", {

  path <- test_setup()
  on.exit(test_teardown(path))
  withr::local_dir(path)

  mod <- file.path(path, "analysis", "license.Rmd")
  Sys.sleep(1.25)
  cat("edit\n", file = mod, append = TRUE)
  observed <- wflow_run(project = path)
  expect_identical(observed, workflowr:::relative(mod))
})

test_that("wflow_run can run specified file", {

  path <- test_setup()
  on.exit(test_teardown(path))
  withr::local_dir(path)

  mod <- file.path(path, "analysis", "license.Rmd")
  cat("edit\n", file = mod, append = TRUE)
  specified <- file.path(path, "analysis", "index.Rmd")
  observed <- wflow_run(file = specified, project = path)
  expect_identical(observed, workflowr:::relative(specified))
})

test_that("wflow_run argument verbose controls code echoing", {

  path <- test_setup()
  on.exit(test_teardown(path))
  withr::local_dir(path)

  rmd <- file.path(path, "analysis", "license.Rmd")
  cat(c("```{r}\n",
        "1 + 1\n",
        "```\n"),
      file = rmd, append = TRUE)

  # default
  expect_output(wflow_run(rmd, project = path), "1 \\+ 1")
  expect_output(wflow_run(rmd, project = path), "\\[1\\] 2")
  # verbose = TRUE
  expect_output(wflow_run(rmd, verbose = TRUE, project = path), "1 \\+ 1")
  expect_output(wflow_run(rmd, verbose = TRUE, project = path), "\\[1\\] 2")
  # verbose = FALSE
  expect_silent(wflow_run(rmd, verbose = FALSE, project = path))
})

# Test error handling ----------------------------------------------------------

test_that("wflow_run sends warning if working and knit directory mismatch", {

  path <- test_setup()
  on.exit(test_teardown(path))

  expect_warning(
    wflow_run(project = path),
    "Working directory does not match knit_root_dir"
  )
  expect_warning(
    wflow_run(project = path),
    path
  )
})

test_that("wflow_run fails if passed more than one file or non-Rmd", {

  path <- test_setup()
  on.exit(test_teardown(path))

  multiple <- fs::dir_ls(path = file.path(path, "analysis"),
                         glob = "*Rmd")
  expect_error(
    wflow_run(file = multiple, project = path),
    "file must be NULL or a character vector with one filename"
  )

  non_rmd <- file.path(path, "README.md")
  expect_error(
    wflow_run(file = non_rmd, project = path),
    "File extensions must be either Rmd or rmd."
  )
})

test_that("wflow_run fails with obviously bad input", {

  # file
  expect_error(
    wflow_run(file = 1),
    "file must be NULL or a character vector with one filename"
  )
  expect_error(
    wflow_run(file = fs::path_temp()),
    "file cannot be a path to a directory"
  )
  expect_error(
    wflow_run(file = fs::file_temp()),
    "The file must exist. Check the path to the file"
  )

  # verbose
  expect_error(
    wflow_run(verbose = 1),
    "verbose must be a one-element logical vector"
  )

  # project
  expect_error(
    wflow_run(project = 1),
    "project must be a one-element character vector"
  )
  expect_error(
    wflow_run(project = fs::file_temp()),
    "project directory does not exist."
  )
})
