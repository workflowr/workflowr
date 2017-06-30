context("obtain_existing_path")

# Function in R/utility.R

# Setup ------------------------------------------------------------------------

# Create and move to a temporary, nested directory
tmp_dir <- tempfile("test-obtain_existing_path-")
nested_dir <- file.path(tmp_dir, "nested")
cwd <- getwd()
on.exit(setwd(cwd))
on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
dir.create(nested_dir, recursive = TRUE)
setwd(nested_dir)

# Test obtain_existing_path ----------------------------------------------------

test_that("obtain_existing_paths works with nested non-existing directories", {
  expect_identical(obtain_existing_path("x"), workflowr:::normalizePath("."))
  expect_identical(obtain_existing_path("x/"), workflowr:::normalizePath("."))
  expect_identical(obtain_existing_path("x/y"), workflowr:::normalizePath("."))
  expect_identical(obtain_existing_path("x/y/"), workflowr:::normalizePath("."))
  expect_identical(obtain_existing_path("x/y/z"), workflowr:::normalizePath("."))
})

test_that("obtain_existing_paths works with relative path to cwd", {
expect_identical(obtain_existing_path("./x"), workflowr:::normalizePath("."))
expect_identical(obtain_existing_path("./x/"), workflowr:::normalizePath("."))
expect_identical(obtain_existing_path("./x/y"), workflowr:::normalizePath("."))
expect_identical(obtain_existing_path("./x/y/"), workflowr:::normalizePath("."))
expect_identical(obtain_existing_path("./x/y/z"), workflowr:::normalizePath("."))
})

test_that("obtain_existing_paths works with relative path to upstream", {
expect_identical(obtain_existing_path("../x"), workflowr:::normalizePath(".."))
expect_identical(obtain_existing_path("../x/"), workflowr:::normalizePath(".."))
expect_identical(obtain_existing_path("../x/y"), workflowr:::normalizePath(".."))
expect_identical(obtain_existing_path("../x/y/"), workflowr:::normalizePath(".."))
expect_identical(obtain_existing_path("../x/y/z"), workflowr:::normalizePath(".."))
})

test_that("obtain_existing_paths works with relative path to two upstream", {
expect_identical(obtain_existing_path("../../x"), workflowr:::normalizePath("../.."))
expect_identical(obtain_existing_path("../../x/"), workflowr:::normalizePath("../.."))
expect_identical(obtain_existing_path("../../x/y"), workflowr:::normalizePath("../.."))
expect_identical(obtain_existing_path("../../x/y/"), workflowr:::normalizePath("../.."))
expect_identical(obtain_existing_path("../../x/y/z"), workflowr:::normalizePath("../.."))
})

test_that("obtain_existing_paths works with relative path to home directory", {
expect_identical(obtain_existing_path("~/x"), workflowr:::normalizePath("~"))
expect_identical(obtain_existing_path("~/x/"), workflowr:::normalizePath("~"))
expect_identical(obtain_existing_path("~/x/y"), workflowr:::normalizePath("~"))
expect_identical(obtain_existing_path("~/x/y/"), workflowr:::normalizePath("~"))
expect_identical(obtain_existing_path("~/x/y/z"), workflowr:::normalizePath("~"))
})
