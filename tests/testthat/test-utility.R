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

# Test relpath -----------------------------------------------------------------

test_that("relpath works on basic input", {
  file1 = "~/git/ash/analysis"
  file2 = "~/git/ash/code/dsc-shrink/datamakers"
  expected <- "../code/dsc-shrink/datamakers"
  actual <- relpath(file2, file1)
  expect_identical(actual, expected)
})
