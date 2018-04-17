context("build_rmd_external")

# Setup ------------------------------------------------------------------------

# Create a temporary workflowr project
tmp_dir <- tempfile("build_rmd_external-")
cwd <- getwd()
on.exit(setwd(cwd))
on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
dir.create(tmp_dir)
tmp_dir <- workflowr:::absolute(tmp_dir)

# Copy test files
file.copy("files/test-wflow_build/.", tmp_dir, recursive = TRUE)

# Create empty website files to satisfy rmarkdown::render_site (called by
# build_rmd). HTML files written to _site/
file.create(file.path(tmp_dir, "_site.yml"))
file.create(file.path(tmp_dir, "index.Rmd"))

# Directory to write log files
l <- "../log"

setwd(tmp_dir)

# Test build_rmd_external ------------------------------------------------------

test_that("log_dir is created if non-existent", {

  skip_on_cran()

  unlink(l, recursive = TRUE, force = TRUE) # just to be safe
  on.exit(unlink(l, recursive = TRUE, force = TRUE))
  expect_message(build_rmd_external("seed.Rmd", seed = 1, log_dir = l),
                 sprintf("log directory created: %s", l))
  expect_true(dir.exists(l))
})

test_that("log files are created", {

  skip_on_cran()

  on.exit(unlink(l, recursive = TRUE, force = TRUE))
  build_rmd_external("seed.Rmd", seed = 1, log_dir = l)
  expect_true(length(Sys.glob(file.path(l, "seed.Rmd-*-out.txt"))) == 1)
  expect_true(length(Sys.glob(file.path(l, "seed.Rmd-*-err.txt"))) == 1)
})

test_that("seed is set when file is built", {

  skip_on_cran()

  on.exit(unlink(l, recursive = TRUE, force = TRUE))
  for (s in 1:3) {
    build_rmd_external("seed.Rmd", seed = s, log_dir = l)
    set.seed(s)
    expected <- sprintf("The random number is %d", sample(1:1000, 1))
    lines <- readLines("_site/seed.html")
    observed <- stringr::str_extract(lines, expected)
    observed <- observed[!is.na(observed)]
    expect_identical(observed, expected)
  }
})

test_that("An error stops execution, does not create file,
          and provides informative error message", {

  skip_on_cran()

  on.exit(unlink(l, recursive = TRUE, force = TRUE))
  expect_error(utils::capture.output(
    build_rmd_external("error.Rmd", seed = 1, log_dir = l)),
               "There was an error")
  expect_false(file.exists("_site/error.html"))
  stderr_lines <- readLines(Sys.glob(file.path(l, "error.Rmd-*-err.txt")))
  expect_true(any(grepl("There was an error", stderr_lines)))
})

test_that("A warning does not cause any problem", {

  skip_on_cran()

  on.exit(unlink(l, recursive = TRUE, force = TRUE))
  dir.create(l)
  expect_silent(build_rmd_external("warning.Rmd", seed = 1, log_dir = l))
  expect_true(file.exists("_site/warning.html"))
})

# Test error handling ----------------------------------------------------------

test_that("rmd is valid", {
  on.exit(unlink(l, recursive = TRUE, force = TRUE))
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
  on.exit(unlink(l, recursive = TRUE, force = TRUE))
  expect_error(build_rmd_external("seed.Rmd", "1", log_dir = l),
               "seed must be a one element numeric vector")
  expect_error(build_rmd_external("seed.Rmd", 1:10, log_dir = l),
               "seed must be a one element numeric vector")
})

test_that("log_dir is valid", {
  on.exit(unlink(l, recursive = TRUE, force = TRUE))
  expect_error(build_rmd_external("seed.Rmd", 1, log_dir = 1),
               "log_dir must be a one element character vector")
  expect_error(build_rmd_external("seed.Rmd", 1, log_dir = c("a", "b")),
               "log_dir must be a one element character vector")
})
