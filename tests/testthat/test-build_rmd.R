context("build_rmd")

# Setup ------------------------------------------------------------------------

# Create a temporary workflowr project
tmp_dir <- tempfile("build_rmd-")
analysis_dir <- file.path(tmp_dir, "analysis")
docs_dir <- file.path(tmp_dir, "docs")
cwd <- getwd()
on.exit(setwd(cwd))
on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
suppressMessages(wflow_start(tmp_dir, change_wd = FALSE))

# Copy test files
file.copy("files/test-wflow_build_/.", analysis_dir, recursive = TRUE)

setwd(analysis_dir)

# Test build_rmd ---------------------------------------------------------------

test_that("seed is set when file is built", {
  for (s in 1:3) {
    build_rmd("seed.Rmd", seed = s, quiet = TRUE)
    set.seed(s)
    expected <- sprintf("<pre><code>## The random number is %d</code></pre>",
                        sample(1:1000, 1))
    observed <- grep("<pre><code>## The random number is ",
                     readLines(file.path(docs_dir, "seed.html")),
                     value = TRUE)
    expect_identical(observed, expected)
  }
})

test_that("An error breaks it", {
  expect_error(build_rmd("error.Rmd", seed = 1, quiet = TRUE),
               "There was an error")
  expect_false(file.exists(file.path(docs_dir, "error.html")))
})

test_that("A warning does not cause any problem", {
  expect_silent(build_rmd("warning.Rmd", seed = 1, quiet = TRUE))
  expect_true(file.exists(file.path(docs_dir, "warning.html")))
})

# Test error handling ----------------------------------------------------------

test_that("rmd is valid", {
  expect_error(build_rmd(1, 1),
               "rmd must be a one element character vector")
  expect_error(build_rmd(c("file1.Rmd", "file2.Rmd"), 1),
               "rmd must be a one element character vector")
  expect_error(build_rmd(list("file1.Rmd", "file2.Rmd"), 1),
               "rmd must be a one element character vector")
  expect_error(build_rmd("file1.Rmd", 1),
               "rmd must exist")
})

test_that("seed is valid", {
  expect_error(build_rmd("seed.Rmd", "1"),
               "seed must be a one element numeric vector")
  expect_error(build_rmd("seed.Rmd", 1:10),
               "seed must be a one element numeric vector")
})
