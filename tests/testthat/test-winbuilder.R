context("winbuilder")

# Attempting to diagnose winbuilder-specific errors

test_that("fs::dir_exists returns FALSE for non-existent directories starting at root", {
  expect_false(fs::dir_exists("/a/b/c"))
  expect_identical(dirname("/x"), "/")
  expect_identical(fs::path_abs(dirname("/x")), path_abs("/"))
})

test_that("git2r can add and commit a file", {

  if (packageVersion("git2r") <= as.numeric_version("0.21.0"))
    skip("requires S3 version of git2r")

  # Adapting the example from ?git2r::commit
  path <- tempfile(pattern = "git2r-")
  dir.create(path)
  repo <- git2r::init(path)
  git2r::config(repo, user.name = "Alice", user.email = "alice@example.org")
  writeLines("Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do",
             file.path(path, "example.txt"))
  git2r::add(repo, "example.txt")
  commit_1 <- git2r::commit(repo, "First commit message")
  expect_identical(class(commit_1), "git_commit")
})
