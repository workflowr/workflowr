context("utility")

# Test to_html -----------------------------------------------------------------

test_that("to_html converts file extension Rmd", {
  expected <- "file.html"
  actual <- to_html("file.Rmd")
  expect_identical(actual, expected)
})

test_that("to_html converts file extension rmd", {
  expected <- "file.html"
  actual <- to_html("file.rmd")
  expect_identical(actual, expected)
})

test_that("to_html converts file extension even if it also appears in filename", {
  expected <- "Rmd.html"
  actual <- to_html("Rmd.Rmd")
  expect_identical(actual, expected)
})

test_that("to_html converts simple absolute path", {
  docs <- "/home/user/project/docs"
  expected <- file.path(docs, "file.html")
  actual <- to_html("/home/user/project/analysis/file.Rmd", outdir = docs)
  expect_identical(actual, expected)
})

test_that("to_html converts simple relative path", {
  docs <- "docs"
  expected <- file.path(docs, "file.html")
  actual <- to_html("analysis/file.Rmd", outdir = docs)
  expect_identical(actual, expected)
})

test_that("to_html does not prepend ./", {
  docs <- "."
  expected <- "file.html"
  actual <- to_html("analysis/file.Rmd", outdir = docs)
  expect_identical(actual, expected)
})

test_that("to_html is vectorized", {
  docs <- "/home/user/project/docs"
  expected <- file.path(docs, c("1.html", "2.html", "3.html"))
  actual <- to_html(c("1.Rmd", "2.Rmd", "3.Rmd"),
                    outdir = docs)
  expect_identical(actual, expected)
})

test_that("to_html handles trailing slashes", {
  docs <- "/home/user/project/docs/"
  expected <- "/home/user/project/docs/file.html"
  actual <- to_html("/home/user/project/analysis/file.Rmd", outdir = docs)
  expect_identical(actual, expected)
})

test_that("to_html throws errors for invalid extensions", {
  expect_error(to_html("file.md"), "Invalid file extension")
  expect_error(to_html("file.z"), "Invalid file extension")
  expect_error(to_html("file"), "Invalid file extension")
})

# Test commonprefix ------------------------------------------------------------

# Warning: The implementation of commonprefix is substantially different from
# its Python version
test_that("commonprefix finds common ", {
  expect_identical(
    commonprefix(c("a", "b", "c"), c("a", "b", "c")),
    c("a", "b", "c")
  )
  expect_identical(
    commonprefix(c("a", "b", "c"), c("a", "b", "z")),
    c("a", "b")
  )
  expect_identical(
    commonprefix(c("a", "b", "c"), c("a", "y", "z")),
    "a"
  )
  expect_identical(
    commonprefix(c("a", "b", "c"), c("x", "y", "z")),
    character()
  )
})

test_that("commonprefix handles edge cases", {
  expect_identical(commonprefix("a", "a"), "a")
  expect_identical(commonprefix("a", "b"), character())
  expect_identical(commonprefix(character(), "a"), character())
  expect_identical(commonprefix("a", character()), character())
  expect_identical(commonprefix(character(), character()), character())
  expect_identical(commonprefix("", "a"), character())
  expect_identical(commonprefix("a", ""), character())
  expect_identical(commonprefix("", ""), "")
})

# Test relpath -----------------------------------------------------------------

test_that("relpath returns subdirectory", {
  path = "/test/location/project"
  start = "/test/location"
  expected <- "project"
  actual <- relpath(path, start)
  expect_identical(actual, expected)
})

test_that("relpath returns nested subdirectories", {
  path = "/test/location/project/sub1/sub2"
  start = "/test/location"
  expected <- "project/sub1/sub2"
  actual <- relpath(path, start)
  expect_identical(actual, expected)
})

test_that("relpath returns upstream directory", {
  path = "/test"
  start = "/test/location"
  expected <- ".."
  actual <- relpath(path, start)
  expect_identical(actual, expected)
})

test_that("relpath returns multiple upstream directories", {
  path = "/test"
  start = "/test/location/project"
  expected <- "../.."
  actual <- relpath(path, start)
  expect_identical(actual, expected)
})

test_that("relpath returns . when directories are the same", {
  path = "/test/location/project"
  start = "/test/location/project"
  expected <- "."
  actual <- relpath(path, start)
  expect_identical(actual, expected)
})

test_that("relpath returns files in upstream directories", {
  path = "/test/location/project/sub1/file"
  start = "/test/location/project/sub2"
  expected <- "../sub1/file"
  actual <- relpath(path, start)
  expect_identical(actual, expected)
})

test_that("relpath thows error for tilde", {
  expect_error(relpath("~/path", "/path"),
               "No tilde allowed in path or start")
  expect_error(relpath("/path", "~/path"),
               "No tilde allowed in path or start")
})

# Test relpath_vec -------------------------------------------------------------

test_that("relpath_vec works on vector input", {
  path = c("/test", "/test/location/subdir")
  start = "/test/location"
  expected <- c("..", "subdir")
  actual <- relpath_vec(path, start)
  expect_identical(actual, expected)
})

test_that("relpath_vec works on relative paths to existing files", {
  dir.create("x/y/z", recursive = TRUE)
  on.exit(unlink("x", recursive = TRUE))
  path = c("x", "x/y/z")
  start = "./x/y"
  expected <- c("..", "z")
  actual <- relpath_vec(path, start)
  expect_identical(actual, expected)
})

test_that("relpath_vec is backwards compatible with relpath", {
  path = "/test"
  start = "/test/location"
  expected <- ".."
  actual <- relpath_vec(path, start)
  expect_identical(actual, expected)
})
