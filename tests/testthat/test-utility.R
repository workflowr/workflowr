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
  actual <- workflowr:::to_html("/home/user/project/analysis/file.Rmd",
                                outdir = docs)
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

# Test absolute ----------------------------------------------------------------

test_that("absolute expands existing file", {
  path_rel <- "test-utility.R"
  path_abs <- workflowr:::absolute(path_rel)
  expect_true(R.utils::isAbsolutePath(path_abs))
})

test_that("absolute expands existing directory", {
  path_rel <- "."
  path_abs <- workflowr:::absolute(path_rel)
  expect_true(R.utils::isAbsolutePath(path_abs))
})

test_that("absolute expands non-existent file", {
  path_rel <- "non-existent-file"
  path_abs <- workflowr:::absolute(path_rel)
  expect_true(R.utils::isAbsolutePath(path_abs))
})

test_that("absolute expands non-existent directory", {
  path_rel <- "a/b/c"
  path_abs <- workflowr:::absolute(path_rel)
  expect_true(R.utils::isAbsolutePath(path_abs))
})

test_that("absolute removes duplicated forward slashes", {
  path_rel <- "a//b/c"
  path_abs <- workflowr:::absolute(path_rel)
  expect_false(stringr::str_detect(path_abs, "//"))
})

test_that("absolute removes duplicated back slashes", {
  path_rel <- "a\\\\b/c"
  path_abs <- workflowr:::absolute(path_rel)
  expect_false(stringr::str_detect(path_abs, "\\\\"))
})

test_that("absolute removes trailing forward slash(es)", {
  path_rel <- c("a/b/c/", "a/b/c//")
  path_abs <- workflowr:::absolute(path_rel)
  expect_false(all(stringr::str_detect(path_abs, "/$")))
})

test_that("absolute removes trailing back slash(es)", {
  path_rel <- c("a\\b\\c\\", "a\\b\\c\\\\")
  path_abs <- workflowr:::absolute(path_rel)
  expect_false(all(stringr::str_detect(path_abs, "\\$")))
})

test_that("absolute converts back slashes to forward slashes", {
  path_rel <- c("a\\b\\c\\", "a\\\\b\\\\c\\\\")
  path_abs <- workflowr:::absolute(path_rel)
  expect_false(all(stringr::str_detect(path_abs, "\\\\")))
})

test_that("absolute does not add any attributes to the character vector", {
  path_rel <- c("a/b/c", "x/y/z")
  path_abs <- workflowr:::absolute(path_rel)
  expect_true(is.character(path_abs))
  expect_true(is.null(attributes(path_abs)))
})

test_that("absolute returns NULL for NULL", {
  expect_identical(workflowr:::absolute(NULL), NULL)
})

test_that("absolute returns NA for NA", {
  expect_identical(workflowr:::absolute(NA), NA)
})

# Test relative -----------------------------------------------------------------

test_that("relative returns subdirectory", {
  path = "/test/location/project"
  start = "/test/location"
  expected <- "project"
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative returns nested subdirectories", {
  path = "/test/location/project/sub1/sub2"
  start = "/test/location"
  expected <- "project/sub1/sub2"
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative returns upstream directory", {
  path = "/test"
  start = "/test/location"
  expected <- ".."
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative returns multiple upstream directories", {
  path = "/test"
  start = "/test/location/project"
  expected <- "../.."
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative returns . when directories are the same", {
  path = "/test/location/project"
  start = "/test/location/project"
  expected <- "."
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative returns files in upstream directories", {
  path = "/test/location/project/sub1/file"
  start = "/test/location/project/sub2"
  expected <- "../sub1/file"
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative can handle tilde for home directory", {
  path = "~/test/location/project/sub1/file"
  start = "~/test/location/project/sub2"
  expected <- "../sub1/file"
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative can handle a tilde in an absolute path", {
  path = "/test/location/project/sub1/file~"
  start = "/test/location/project/sub2"
  expected <- "../sub1/file~"
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative returns NULL for NULL", {
  expect_identical(workflowr:::relative(NULL), NULL)
})

test_that("relative returns NA for NA", {
  expect_identical(workflowr:::relative(NA), NA)
})

test_that("relative works on vector input", {
  path = c("/test", "/test/location/subdir")
  start = "/test/location"
  expected <- c("..", "subdir")
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative works on relative paths to existing files", {
  dir.create("x/y/z", recursive = TRUE)
  on.exit(unlink("x", recursive = TRUE, force = TRUE))
  path = c("x", "x/y/z")
  start = "./x/y"
  expected <- c("..", "z")
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative handles NA and NULL", {
  expect_null(workflowr:::relative(NULL))
  expect_identical(NA, workflowr:::relative(NA))
  path = c("/test", NA, NULL, "/test/location/subdir")
  start = "/test/location"
  expected <- c("..", NA, NULL, "subdir")
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})
