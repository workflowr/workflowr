context("utility")

# Test to_html -----------------------------------------------------------------

test_that("to_html converts file extension Rmd", {
  expected <- "file.html"
  actual <- workflowr:::to_html("file.Rmd")
  expect_identical(actual, expected)
})

test_that("to_html converts file extension rmd", {
  expected <- "file.html"
  actual <- workflowr:::to_html("file.rmd")
  expect_identical(actual, expected)
})

test_that("to_html converts file extension even if it also appears in filename", {
  expected <- "Rmd.html"
  actual <- workflowr:::to_html("Rmd.Rmd")
  expect_identical(actual, expected)
})

test_that("to_html converts simple absolute path", {
  docs <- "/home/user/project/docs"
  expected <- file.path(docs, "file.html")
  actual <- workflowr:::to_html("/home/user/project/analysis/file.Rmd", outdir = docs)
  expect_identical(actual, expected)
})

test_that("to_html converts simple relative path", {
  docs <- "docs"
  expected <- file.path(docs, "file.html")
  actual <- workflowr:::to_html("analysis/file.Rmd", outdir = docs)
  expect_identical(actual, expected)
})

test_that("to_html does not prepend ./", {
  docs <- "."
  expected <- "file.html"
  actual <- workflowr:::to_html("analysis/file.Rmd", outdir = docs)
  expect_identical(actual, expected)
})

test_that("to_html is vectorized", {
  docs <- "/home/user/project/docs"
  expected <- file.path(docs, c("1.html", "2.html", "3.html"))
  actual <- workflowr:::to_html(c("1.Rmd", "2.Rmd", "3.Rmd"),
                    outdir = docs)
  expect_identical(actual, expected)
})

test_that("to_html handles trailing slashes", {
  docs <- "/home/user/project/docs/"
  expected <- "/home/user/project/docs/file.html"
  actual <- workflowr:::to_html("/home/user/project/analysis/file.Rmd",
                                outdir = docs)
  expect_identical(actual, expected)
})

test_that("to_html throws errors for invalid extensions", {
  expect_error(workflowr:::to_html("file.md"), "Invalid file extension")
  expect_error(workflowr:::to_html("file.z"), "Invalid file extension")
  expect_error(workflowr:::to_html("file"), "Invalid file extension")
})

# Test commonprefix ------------------------------------------------------------

# Warning: The implementation of commonprefix is substantially different from
# its Python version
test_that("commonprefix finds common ", {
  expect_identical(
    workflowr:::commonprefix(c("a", "b", "c"), c("a", "b", "c")),
    c("a", "b", "c")
  )
  expect_identical(
    workflowr:::commonprefix(c("a", "b", "c"), c("a", "b", "z")),
    c("a", "b")
  )
  expect_identical(
    workflowr:::commonprefix(c("a", "b", "c"), c("a", "y", "z")),
    "a"
  )
  expect_identical(
    workflowr:::commonprefix(c("a", "b", "c"), c("x", "y", "z")),
    character()
  )
})

test_that("commonprefix handles edge cases", {
  expect_identical(workflowr:::commonprefix("a", "a"), "a")
  expect_identical(workflowr:::commonprefix("a", "b"), character())
  expect_identical(workflowr:::commonprefix(character(), "a"), character())
  expect_identical(workflowr:::commonprefix("a", character()), character())
  expect_identical(workflowr:::commonprefix(character(), character()), character())
  expect_identical(workflowr:::commonprefix("", "a"), character())
  expect_identical(workflowr:::commonprefix("a", ""), character())
  expect_identical(workflowr:::commonprefix("", ""), "")
})

# Test relpath -----------------------------------------------------------------

test_that("relpath returns subdirectory", {
  path = "/test/location/project"
  start = "/test/location"
  expected <- "project"
  actual <- workflowr:::relpath(path, start)
  expect_identical(actual, expected)
})

test_that("relpath returns nested subdirectories", {
  path = "/test/location/project/sub1/sub2"
  start = "/test/location"
  expected <- "project/sub1/sub2"
  actual <- workflowr:::relpath(path, start)
  expect_identical(actual, expected)
})

test_that("relpath returns upstream directory", {
  path = "/test"
  start = "/test/location"
  expected <- ".."
  actual <- workflowr:::relpath(path, start)
  expect_identical(actual, expected)
})

test_that("relpath returns multiple upstream directories", {
  path = "/test"
  start = "/test/location/project"
  expected <- "../.."
  actual <- workflowr:::relpath(path, start)
  expect_identical(actual, expected)
})

test_that("relpath returns . when directories are the same", {
  path = "/test/location/project"
  start = "/test/location/project"
  expected <- "."
  actual <- workflowr:::relpath(path, start)
  expect_identical(actual, expected)
})

test_that("relpath returns files in upstream directories", {
  path = "/test/location/project/sub1/file"
  start = "/test/location/project/sub2"
  expected <- "../sub1/file"
  actual <- workflowr:::relpath(path, start)
  expect_identical(actual, expected)
})

test_that("relpath thows error for relative path with tilde", {
  expect_error(workflowr:::relpath("~/path", "/path"),
               "arguments path and start cannot begin with a tilde")
  expect_error(workflowr:::relpath("/path", "~/path"),
               "arguments path and start cannot begin with a tilde")
})

test_that("relpath can handle a tilde in an absolute path", {
  path = "/test/location/project/sub1/file~"
  start = "/test/location/project/sub2"
  expected <- "../sub1/file~"
  actual <- workflowr:::relpath(path, start)
  expect_identical(actual, expected)
})

test_that("relpath returns NULL for NULL", {
  expect_identical(workflowr:::relpath(NULL), NULL)
})

test_that("relpath returns NA for NA", {
  expect_identical(workflowr:::relpath(NA), NA)
})

# Test relpath_vec -------------------------------------------------------------

test_that("relpath_vec works on vector input", {
  path = c("/test", "/test/location/subdir")
  start = "/test/location"
  expected <- c("..", "subdir")
  actual <- workflowr:::relpath_vec(path, start)
  expect_identical(actual, expected)
})

test_that("relpath_vec works on relative paths to existing files", {
  dir.create("x/y/z", recursive = TRUE)
  on.exit(unlink("x", recursive = TRUE, force = TRUE))
  path = c("x", "x/y/z")
  start = "./x/y"
  expected <- c("..", "z")
  actual <- workflowr:::relpath_vec(path, start)
  expect_identical(actual, expected)
})

test_that("relpath_vec is backwards compatible with relpath", {
  path = "/test"
  start = "/test/location"
  expected <- ".."
  actual <- workflowr:::relpath_vec(path, start)
  expect_identical(actual, expected)
})

test_that("relpath_vec accepts NA and NULL only if some valid input included", {
  expect_error(workflowr:::relpath_vec(NULL), "path must be a character vector")
  expect_error(workflowr:::relpath_vec(NA), "path must be a character vector")
  path = c("/test", NA, NULL, "/test/location/subdir")
  start = "/test/location"
  expected <- c("..", NA, NULL, "subdir")
  actual <- workflowr:::relpath_vec(path, start)
  expect_identical(actual, expected)
})
