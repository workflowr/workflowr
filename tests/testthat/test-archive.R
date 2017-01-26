context("archive")

library("git2r")

# Set up a temporary directory
site_dir <- tempfile("test-archive")
dir.create(site_dir)
cwd <- getwd()
setwd(site_dir)

# Some toy objects
x <- 1
y <- "hello"
z <- 1:3

files_expected <- paste0(c("x", "y", "z"), "-abc.rds")

test_that("archive can save objects", {
  archive(x, y, z, id = "abc", location = ".")
  expect_true(all(file.exists(files_expected)))
  expect_true(all(file.size(files_expected) > 0))
})

rest1 <- restore(x, y, z, location = ".")

test_that("restore can load objects", {
  expect_true(all(rest1$x$abc == x,
                  rest1$y$abc == y,
                  rest1$z$abc == z))
})

rm(x, y, z)
rest2 <- restore(x, y, z, location = ".")

test_that("restore can load objects when they don't exist", {
  expect_true(all(rest2$x$abc == rest1$x$abc,
                  rest2$y$abc == rest1$y$abc,
                  rest2$z$abc == rest1$z$abc))
})

# Update toy objects and archive
x <- 2
y <- "hi"
z <- -1:-2
archive(x, y, z, id = "xyz", location = ".")
rest3 <- restore(x, y, z, location = ".")

test_that("restore can load multiple versions of an object", {
  expect_true(all(rest3$x$abc == rest2$x$abc,
                  rest3$y$abc == rest2$y$abc,
                  rest3$z$abc == rest2$z$abc,
                  rest3$x$xyz == x,
                  rest3$y$xyz == y,
                  rest3$z$xyz == z))
})

x <- 5
archive(x, id = "solo", location = ".")
rest4 <- restore(x, y, z, location = ".")

test_that("restore can load objects with different number of versions", {
  expect_true(all(rest4$x$solo == x,
                  rest4$x$abc == rest3$x$abc,
                  rest4$y$abc == rest3$y$abc,
                  rest4$z$abc == rest3$z$abc,
                  rest4$x$xyz == rest3$x$xyz,
                  rest4$y$xyz == rest3$y$xyz,
                  rest4$z$xyz == rest3$z$xyz))
})

test_that("archive throws error if missing object", {
  expect_error(archive(a, y, id = "missing", location = "."),
               "object 'a' not found")
  # Order of missing object doesn't matter
  expect_error(archive(y, a, id = "missing", location = "."),
               "object 'a' not found")
})

test_that("restore sends warning if no archive available and returns NA", {
  expect_warning(rest5 <- restore(a, y, location = "."),
                 "Could not find archived values of a in .")
  expect_identical(rest5$a, NA)
})

# To do:
# overwriting file
# error handling (no spaces or dashes in id)

setwd(cwd)
unlink(site_dir)
