context("build_rmd")

# Setup ------------------------------------------------------------------------

# Create a temporary directory
tmp_dir <- tempfile("build_rmd-")
cwd <- getwd()
on.exit(setwd(cwd))
on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
fs::dir_create(tmp_dir)
tmp_dir <- workflowr:::absolute(tmp_dir)

# Copy test files
file.copy("files/test-wflow_build/.", tmp_dir, recursive = TRUE)

# Create empty website files to satisfy rmarkdown::render_site (called by
# build_rmd). HTML files written to _site/
fs::file_create(file.path(tmp_dir, "_site.yml"))
fs::file_create(file.path(tmp_dir, "index.Rmd"))

setwd(tmp_dir)

# Test build_rmd ---------------------------------------------------------------

test_that("seed is set when file is built", {

  skip_on_cran()

  for (s in 1:3) {
    build_rmd("seed.Rmd", seed = s, quiet = TRUE)
    set.seed(s)
    expected <- sprintf("The random number is %d", sample(1:1000, 1))
    lines <- readLines("_site/seed.html")
    observed <- stringr::str_extract(lines, expected)
    observed <- observed[!is.na(observed)]
    expect_identical(observed, expected)
  }
})

test_that("An error breaks it", {

  skip_on_cran()

  expect_error(build_rmd("error.Rmd", seed = 1, quiet = TRUE),
               "There was an error")
  expect_false(fs::file_exists("_site/error.html"))
})

test_that("A warning does not cause any problem", {

  skip_on_cran()

  expect_silent(build_rmd("warning.Rmd", seed = 1, quiet = TRUE))
  expect_true(fs::file_exists("_site/warning.html"))
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
