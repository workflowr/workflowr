context("build_rmd_external")

# Setup ------------------------------------------------------------------------

# Create a temporary workflowr project
tmp_dir <- tempfile("build_rmd_external-")
analysis_dir <- file.path(tmp_dir, "analysis")
docs_dir <- file.path(tmp_dir, "docs")
cwd <- getwd()
on.exit(setwd(cwd))
on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
suppressMessages(wflow_start(tmp_dir, change_wd = FALSE))

# Copy test files
file.copy("files/test-wflow_build_/.", analysis_dir, recursive = TRUE)

# Directory to write log files
l <- "../log"

setwd(analysis_dir)

# Test build_rmd_external ------------------------------------------------------

test_that("log_dir is created if non-existent", {
  unlink(l, recursive = TRUE) # just to be safe
  on.exit(unlink(l, recursive = TRUE))
  expect_message(build_rmd_external("seed.Rmd", seed = 1, log_dir = l),
                 sprintf("log directory created: %s", l))
  expect_true(dir.exists(l))
})

test_that("log files are created", {
  on.exit(unlink(l, recursive = TRUE))
  build_rmd_external("seed.Rmd", seed = 1, log_dir = l)
  expect_true(length(Sys.glob(file.path(l, "seed.Rmd-*-out.txt"))) == 1)
  expect_true(length(Sys.glob(file.path(l, "seed.Rmd-*-err.txt"))) == 1)
})

test_that("seed is set when file is built", {
  on.exit(unlink(l, recursive = TRUE))
  for (s in 1:3) {
    build_rmd_external("seed.Rmd", seed = s, log_dir = l)
    set.seed(s)
    expected <- sprintf("<pre><code>## The random number is %d</code></pre>",
                        sample(1:1000, 1))
    observed <- grep("<pre><code>## The random number is ",
                     readLines(file.path(docs_dir, "seed.html")),
                     value = TRUE)
    expect_identical(observed, expected)
  }
})

test_that("An error returns FALSE, does not create file,
          and logs error in error file", {
  on.exit(unlink(l, recursive = TRUE))
  return_value <- build_rmd_external("error.Rmd", seed = 1, log_dir = l)
  expect_identical(return_value, FALSE)
  expect_false(file.exists(file.path(docs_dir, "error.html")))
  expect_true(any(grepl("There was an error",
              readLines(Sys.glob(file.path(l, "error.Rmd-*-err.txt"))))))
})

test_that("A warning does not cause any problem", {
  on.exit(unlink(l, recursive = TRUE))
  dir.create(l)
  expect_silent(build_rmd_external("warning.Rmd", seed = 1, log_dir = l))
  expect_true(file.exists(file.path(docs_dir, "warning.html")))
})

# Test error handling ----------------------------------------------------------

test_that("rmd is valid", {
  on.exit(unlink(l, recursive = TRUE))
  expect_error(build_rmd_external(1, 1, log_dir = l),
               "rmd must be a one element character vector")
  expect_error(build_rmd_external(c("file1.Rmd", "file2.Rmd"), 1, log_dir = l),
               "rmd must be a one element character vector")
  expect_error(build_rmd_external(list("file1.Rmd", "file2.Rmd"), 1, log_dir = l),
               "rmd must be a one element character vector")
  expect_error(build_rmd_external("file1.Rmd", 1, log_dir = l),
               "rmd does not exist: file1.Rmd")
})

test_that("seed is valid", {
  on.exit(unlink(l, recursive = TRUE))
  expect_error(build_rmd_external("seed.Rmd", "1", log_dir = l),
               "seed must be a one element numeric vector")
  expect_error(build_rmd_external("seed.Rmd", 1:10, log_dir = l),
               "seed must be a one element numeric vector")
})

test_that("log_dir is valid", {
  on.exit(unlink(l, recursive = TRUE))
  expect_error(build_rmd_external("seed.Rmd", 1, log_dir = 1),
               "log_dir must be a one element character vector")
  expect_error(build_rmd_external("seed.Rmd", 1, log_dir = c("a", "b")),
               "log_dir must be a one element character vector")
})
