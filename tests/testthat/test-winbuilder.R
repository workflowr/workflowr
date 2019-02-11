context("winbuilder")

# Attempting to diagnose winbuilder-specific errors

if (!(.Platform$OS.type == "windows" && Sys.getenv("USERPROFILE") == "C:\\Users\\CRAN"))
  skip("Only relevant on winbuilder")

test_that("git2r can add and commit a file", {

  if (packageVersion("git2r") <= as.numeric_version("0.21.0"))
    skip("requires S3 version of git2r")

  # Adapting the example from ?git2r::commit
  path <- tempfile(pattern = "git2r-")
  dir.create(path)
  path <- workflowr:::absolute(path)
  repo <- git2r::init(path)
  git2r::config(repo, user.name = "Alice", user.email = "alice@example.org")
  writeLines("Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do",
             file.path(path, "example.txt"))
  git2r::add(repo, "example.txt")
  commit_1 <- git2r::commit(repo, "First commit message")
  expect_identical(class(commit_1), "git_commit")
})

test_that("Obtain winbuilder info on temp directory", {
  expect_identical(tempdir(), "What is the temporary directory path?")
  x <- tempfile()
  expect_identical(x, "What is an example tempfile?")
  fs::file_create(x)
  expect_identical(workflowr:::absolute(x), "What is an example tempfile after absolute?")
  expect_identical(workflowr:::relative(x), "What is an example tempfile after relative?")
  expect_identical(workflowr:::absolute(workflowr:::relative(x)), "What is an example tempfile after relative and absolute?")
})

test_that("Obtain winbuilder info on temp directory (fs)", {
  x <- fs::file_temp()
  expect_identical(x, "(fs) What is an example tempfile?")
  fs::file_create(x)
  expect_identical(fs::path_abs(x), "What is an example tempfile after path_abs?")
  expect_identical(fs::path_rel(x), "What is an example tempfile after path_rel?")
  expect_identical(fs::path_abs(fs::path_rel(x)), "What is an example tempfile after path_rel and path_abs?")
})
