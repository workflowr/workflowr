context("git2r-merge")

# Setup ------------------------------------------------------------------------

library("git2r")

## Create a directory in tempdir
path <- tempfile(pattern="git2r-")
dir.create(path)

## Cleanup
on.exit(unlink(path, recursive=TRUE))

## Initialize a repository
repo <- init(path)
config(repo, user.name="Alice", user.email="alice@example.org")

## Create a file, add and commit
writeLines("Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do",
           con = file.path(path, "test.txt"))
add(repo, "test.txt")
commit_1 <- commit(repo, "Commit message 1")

## Create branch, checkout, change file and commit
b <- checkout(repo, branch = "branch", create = TRUE)
writeLines(c("Lorem ipsum dolor amet sit, consectetur adipisicing elit, sed do",
             "eiusmod tempor incididunt ut labore et dolore magna aliqua."),
           con = file.path(path, "test.txt"))
add(repo, "test.txt")
commit(repo, "Commit message branch")

## Checkout master and create a conflicting change
b <- branches(repo)
checkout(b[sapply(b, "[", "name") == "master"][[1]], force=TRUE)
writeLines(c("Lorem ipsum dolor sit amet, adipisicing consectetur elit, sed do",
             "eiusmod tempor incididunt ut labore et dolore magna aliqua."),
           con = file.path(path, "test.txt"))

# Test merge conflicts ---------------------------------------------------------

test_that("merge unstaged with fail=TRUE", {

  expect_identical(status(repo)$unstaged, list(modified = "test.txt"))

  m <- merge(repo, "branch", fail = TRUE)

  expect_equivalent(class(m), "git_merge_result")
  expect_identical(m$up_to_date, FALSE)
  expect_identical(m$fast_forward, NULL)
  expect_identical(m$conflicts, NULL)
  expect_identical(m$sha, NULL)
})

test_that("merge unstaged with fail=FALSE", {

  expect_identical(status(repo)$unstaged, list(modified = "test.txt"))

  m <- merge(repo, "branch", fail = FALSE)

  expect_equivalent(class(m), "git_merge_result")
  expect_identical(m$up_to_date, FALSE)
  expect_identical(m$fast_forward, NULL)
  expect_identical(m$conflicts, NULL)
  expect_identical(m$sha, NULL)
})

add(repo, "test.txt")

test_that("merge staged with fail=TRUE", {
  expect_identical(status(repo)$staged, list(modified = "test.txt"))

  m <- merge(repo, "branch", fail = TRUE)

  expect_equivalent(class(m), "git_merge_result")
  expect_identical(m$up_to_date, FALSE)
  expect_identical(m$fast_forward, NULL)
  expect_identical(m$conflicts, NULL)
  expect_identical(m$sha, NULL)
})

test_that("merge staged with fail=FALSE", {
  expect_identical(status(repo)$staged, list(modified = "test.txt"))

  m <- merge(repo, "branch", fail = FALSE)

  expect_equivalent(class(m), "git_merge_result")
  expect_identical(m$up_to_date, FALSE)
  expect_identical(m$fast_forward, NULL)
  expect_identical(m$conflicts, NULL)
  expect_identical(m$sha, NULL)
})

commit(repo, "commit conflicting change")

test_that("merge committed with fail = TRUE", {
  expect_identical(status(repo)$unstaged, structure(list(), .Names = character(0)))
  expect_identical(status(repo)$staged, structure(list(), .Names = character(0)))

  m <- merge(repo, "branch", fail = TRUE)

  expect_equivalent(class(m), "git_merge_result")
  expect_identical(m$up_to_date, FALSE)
  expect_identical(m$fast_forward, FALSE)
  expect_identical(m$conflicts, TRUE)
  expect_identical(m$sha, NA_character_)
  # working directory should be clean b/c fail=TRUE
  expect_identical(status(repo)$unstaged, structure(list(), .Names = character(0)))
})

test_that("merge committed with fail = FALSE", {
  expect_identical(status(repo)$unstaged, structure(list(), .Names = character(0)))
  expect_identical(status(repo)$staged, structure(list(), .Names = character(0)))

  m <- merge(repo, "branch", fail = FALSE)

  expect_equivalent(class(m), "git_merge_result")
  expect_identical(m$up_to_date, FALSE)
  expect_identical(m$fast_forward, FALSE)
  expect_identical(m$conflicts, TRUE)
  expect_identical(m$sha, NA_character_)
  # working directory should have merge conflicts as unstaged changes
  expect_identical(status(repo)$unstaged, list(conflicted = "test.txt"))
})
