context("obtain_existing_path")

# Function in R/utility.R

# Setup ------------------------------------------------------------------------

# Create and move to a temporary, nested directory
tmp_dir <- tempfile("test-obtain_existing_path-")
nested_dir <- file.path(tmp_dir, "nested")
cwd <- getwd()
on.exit(setwd(cwd))
on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
fs::dir_create(nested_dir)
tmp_dir <- workflowr:::absolute(tmp_dir)
nested_dir <- workflowr:::absolute(nested_dir)
setwd(nested_dir)

# Test obtain_existing_path ----------------------------------------------------

test_that("obtain_existing_paths works with nested non-existing directories", {
  expect_identical(obtain_existing_path("x"), workflowr:::absolute("."))
  expect_identical(obtain_existing_path("x/"), workflowr:::absolute("."))
  expect_identical(obtain_existing_path("x/y"), workflowr:::absolute("."))
  expect_identical(obtain_existing_path("x/y/"), workflowr:::absolute("."))
  expect_identical(obtain_existing_path("x/y/z"), workflowr:::absolute("."))
})

test_that("obtain_existing_paths works with relative path to cwd", {
expect_identical(obtain_existing_path("./x"), workflowr:::absolute("."))
expect_identical(obtain_existing_path("./x/"), workflowr:::absolute("."))
expect_identical(obtain_existing_path("./x/y"), workflowr:::absolute("."))
expect_identical(obtain_existing_path("./x/y/"), workflowr:::absolute("."))
expect_identical(obtain_existing_path("./x/y/z"), workflowr:::absolute("."))
})

test_that("obtain_existing_paths works with relative path to upstream", {
expect_identical(obtain_existing_path("../x"), workflowr:::absolute(".."))
expect_identical(obtain_existing_path("../x/"), workflowr:::absolute(".."))
expect_identical(obtain_existing_path("../x/y"), workflowr:::absolute(".."))
expect_identical(obtain_existing_path("../x/y/"), workflowr:::absolute(".."))
expect_identical(obtain_existing_path("../x/y/z"), workflowr:::absolute(".."))
})

test_that("obtain_existing_paths works with relative path to two upstream", {
expect_identical(obtain_existing_path("../../x"), workflowr:::absolute("../.."))
expect_identical(obtain_existing_path("../../x/"), workflowr:::absolute("../.."))
expect_identical(obtain_existing_path("../../x/y"), workflowr:::absolute("../.."))
expect_identical(obtain_existing_path("../../x/y/"), workflowr:::absolute("../.."))
expect_identical(obtain_existing_path("../../x/y/z"), workflowr:::absolute("../.."))
})

test_that("obtain_existing_paths works with relative path to home directory", {
expect_identical(obtain_existing_path("~/x"), workflowr:::absolute("~"))
expect_identical(obtain_existing_path("~/x/"), workflowr:::absolute("~"))
expect_identical(obtain_existing_path("~/x/y"), workflowr:::absolute("~"))
expect_identical(obtain_existing_path("~/x/y/"), workflowr:::absolute("~"))
expect_identical(obtain_existing_path("~/x/y/z"), workflowr:::absolute("~"))
})

test_that("obtain_existing_paths works when the root is the upstream existing directory", {

  skip_on_cran()
  # These only fail on winbuilder for some reason
  #
  # -- 1. Failure: obtain_existing_paths works when the root is the upstream existin
  # obtain_existing_path("/x") not identical to workflowr:::absolute("/").
  # 1/1 mismatches
  # x[1]: "D:/x"
  # y[1]: "D:/"
  #
  # -- 2. Failure: obtain_existing_paths works when the root is the upstream existin
  # obtain_existing_path("/x/") not identical to workflowr:::absolute("/").
  # 1/1 mismatches
  # x[1]: "D:/x"
  # y[1]: "D:/"
  #
  # -- 3. Failure: obtain_existing_paths works when the root is the upstream existin
  # obtain_existing_path("/x/y") not identical to workflowr:::absolute("/").
  # 1/1 mismatches
  # x[1]: "D:/x/y"
  # y[1]: "D:/"
  #
  # -- 4. Failure: obtain_existing_paths works when the root is the upstream existin
  # obtain_existing_path("/x/y/") not identical to workflowr:::absolute("/").
  # 1/1 mismatches
  # x[1]: "D:/x/y"
  # y[1]: "D:/"
  #
  # -- 5. Failure: obtain_existing_paths works when the root is the upstream existin
  # obtain_existing_path("/x/y/z") not identical to workflowr:::absolute("/").
  # 1/1 mismatches
  # x[1]: "D:/x/y/z"
  # y[1]: "D:/"

  expect_identical(obtain_existing_path("/x"), workflowr:::absolute("/"))
  expect_identical(obtain_existing_path("/x/"), workflowr:::absolute("/"))
  expect_identical(obtain_existing_path("/x/y"), workflowr:::absolute("/"))
  expect_identical(obtain_existing_path("/x/y/"), workflowr:::absolute("/"))
  expect_identical(obtain_existing_path("/x/y/z"), workflowr:::absolute("/"))
})
