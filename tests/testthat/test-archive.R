context("archive")

library("git2r")

# Test base functions `archive` and `restore` ----------------------------------

# Set up a temporary directory
tmp_dir <- tempfile("test-archive")
dir.create(tmp_dir)
cwd <- getwd()
setwd(tmp_dir)

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

test_that("restore can load objects when they don't exist in .GlobalEnv", {
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

test_that("archive does not overwrite existing files (default)", {
  mtime_pre <- file.mtime(files_expected)
  expect_warning(archive(x, y, z, id = "abc", location = "."),
                 "Archive file already exists: ")
  mtime_post <- file.mtime(files_expected)
  expect_identical(mtime_pre, mtime_post)
})

test_that("archive does overwrite existing files if forced", {
  mtime_pre <- file.mtime(files_expected)
  expect_warning(archive(x, y, z, id = "abc", location = ".",
                         overwrite = TRUE),
                 "Overwriting existing archive file: ")
  mtime_post <- file.mtime(files_expected)
  expect_true(all(mtime_pre < mtime_post))
})

test_that("archive throws errors for invalid input", {
  expect_error(archive(id = "error", location = "id"),
               "No objects to archive")
  expect_error(archive(x, y, z, id = 1, location = "id"),
               "id must be a one element character vector")
  expect_error(archive(x, y, z, id = c("1", "2"), location = "id"),
               "id must be a one element character vector")
  expect_error(archive(x, y, z, id = "test-dash", location = "id"),
               "id cannot contain dashes: ")
  expect_error(archive(x, y, z, id = "test space", location = "id"),
               "id cannot contain spaces: ")
  expect_error(archive(x, y, z, id = "test", location = 1),
               "location must be a one element character vector")
  expect_error(archive(x, y, z, id = "test", location = c("1", "2")),
               "location must be a one element character vector")
  expect_error(archive(x, y, z, id = "test", location = "nonexistent"),
               "location must already exist: ")
  expect_error(archive(x, y, z, id = "test", location = ".", overwrite = 1),
               "overwrite must be one element logical vector")
  expect_error(archive(x, y, z, id = "test", location = ".",
                       overwrite = c(FALSE, TRUE)),
               "overwrite must be one element logical vector")
})

test_that("restore throws errors for invalid input", {
  expect_error(restore(location = "."),
               "No objects to resurrect")
  expect_error(restore(x, y, z, location = 1),
               "location must be a one element character vector")
  expect_error(restore(x, y, z, location = c("1", "2")),
               "location must be a one element character vector")
  expect_error(restore(x, y, z, location =  "nonexistent"),
               "location must already exist: ")
})

setwd(cwd)
unlink(tmp_dir, recursive = TRUE)

# Test wflow_archive and wflow_restore -----------------------------------------

# Set up a temporary workflowr project
site_dir <- tempfile("wflow_archive-")
suppressMessages(wflow_start("Testing wflow_archive", site_dir))
# Add analysis file
file.copy("files/test_archive/archive.Rmd",
          file.path(site_dir, "analysis"))

# Test in progress
test_that("wflow_archive archives files in correct location", {
  wflow_build(files = "archive.Rmd", path = site_dir, quiet = TRUE)
})


unlink(site_dir, recursive = TRUE)
