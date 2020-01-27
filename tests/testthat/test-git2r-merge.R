context("git2r-merge-conflicts")

# Test behavior of git2r merge.git_repository with merge conflicts
#
# Will need to be updated if upstream behavior is fixed:
#
# https://github.com/ropensci/git2r/issues/389
# https://github.com/ropensci/git2r/pull/391

# Setup ------------------------------------------------------------------------

source("setup.R")
library("git2r")

# Test merge conflicts ---------------------------------------------------------

test_that("commited conflicting changes only", {
  path <- test_setup()
  on.exit(test_teardown(path))

  r <- repository(path)
  f <- file.path(path, "test.txt")
  checkout(r, branch = "feature", create = TRUE)
  writeLines("feature branch", con = f)
  add(r, f)
  commit(r, "commit on feature branch")
  checkout(r, "master")
  writeLines("master branch", con = f)
  add(r, f)
  commit(r, "commit on master branch")
  expect_identical(
    status(r)$unstaged,
    structure(list(), .Names = character(0))
  )

  m_fail <- merge(r, "feature", fail = TRUE)
  expect_true(inherits(m_fail, "git_merge_result"))
  expect_identical(m_fail$up_to_date, FALSE)
  expect_identical(m_fail$fast_forward, FALSE)
  expect_identical(m_fail$conflicts, TRUE)
  expect_identical(m_fail$sha, NA_character_)
  expect_identical(
    status(r)$unstaged,
    structure(list(), .Names = character(0))
  )

  m_wflow <- workflowr:::git2r_merge(r, "feature", fail = TRUE)
  expect_true(inherits(m_wflow, "git_merge_result"))
  expect_identical(m_wflow$up_to_date, FALSE)
  expect_identical(m_wflow$fast_forward, FALSE)
  expect_identical(m_wflow$conflicts, TRUE)
  expect_identical(m_wflow$sha, NA_character_)
  expect_identical(
    status(r)$unstaged,
    structure(list(), .Names = character(0))
  )

  m <- merge(r, "feature", fail = FALSE)
  expect_true(inherits(m, "git_merge_result"))
  expect_identical(m$up_to_date, FALSE)
  expect_identical(m$fast_forward, FALSE)
  expect_identical(m$conflicts, TRUE)
  expect_identical(m$sha, NA_character_)
  expect_identical(
    status(r)$unstaged,
    list(conflicted = "test.txt")
  )
})

test_that("staged conflicting changes only", {
  path <- test_setup()
  on.exit(test_teardown(path))

  r <- repository(path)
  f <- file.path(path, "test.txt")
  checkout(r, branch = "feature", create = TRUE)
  writeLines("feature branch", con = f)
  add(r, f)
  commit(r, "commit on feature branch")
  checkout(r, "master")
  writeLines("master branch", con = f)
  add(r, f)
  # leave in staging area w/o committing
  expect_identical(
    status(r)$staged,
    list(new = "test.txt")
  )

  m_fail <- merge(r, "feature", fail = TRUE)
  expect_true(inherits(m_fail, "git_merge_result"))
  expect_identical(m_fail$up_to_date, FALSE)
  expect_identical(m_fail$fast_forward, NULL)
  expect_identical(m_fail$conflicts, NULL)
  expect_identical(m_fail$sha, NULL)
  expect_identical(
    status(r)$staged,
    list(new = "test.txt")
  )

  m_wflow <- workflowr:::git2r_merge(r, "feature", fail = TRUE)
  expect_true(inherits(m_wflow, "git_merge_result"))
  expect_identical(m_wflow$up_to_date, FALSE)
  expect_identical(m_wflow$fast_forward, FALSE)
  expect_identical(m_wflow$conflicts, FALSE)
  expect_identical(m_wflow$sha, NA_character_)
  expect_identical(
    status(r)$staged,
    list(new = "test.txt")
  )

  m <- merge(r, "feature", fail = FALSE)
  expect_true(inherits(m, "git_merge_result"))
  expect_identical(m$up_to_date, FALSE)
  expect_identical(m$fast_forward, NULL)
  expect_identical(m$conflicts, NULL)
  expect_identical(m$sha, NULL)
  expect_identical(
    status(r)$staged,
    list(new = "test.txt")
  )
})

# I tested untracked conflicting changes for convenience, but it is the same
# idea as unstaged changes to a tracked file.
test_that("untracked conflicting changes only", {
  path <- test_setup()
  on.exit(test_teardown(path))

  r <- repository(path)
  f <- file.path(path, "test.txt")
  checkout(r, branch = "feature", create = TRUE)
  writeLines("feature branch", con = f)
  add(r, f)
  commit(r, "commit on feature branch")
  checkout(r, "master")
  writeLines("master branch", con = f)
  # leave as unstaged change
  expect_identical(
    status(r)$untracked,
    list(untracked = "test.txt")
  )

  m_fail <- merge(r, "feature", fail = TRUE)
  expect_true(inherits(m_fail, "git_merge_result"))
  expect_identical(m_fail$up_to_date, FALSE)
  expect_identical(m_fail$fast_forward, NULL)
  expect_identical(m_fail$conflicts, NULL)
  expect_identical(m_fail$sha, NULL)
  expect_identical(
    status(r)$untracked,
    list(untracked = "test.txt")
  )

  m_wflow <- workflowr:::git2r_merge(r, "feature", fail = TRUE)
  expect_true(inherits(m_wflow, "git_merge_result"))
  expect_identical(m_wflow$up_to_date, FALSE)
  expect_identical(m_wflow$fast_forward, FALSE)
  expect_identical(m_wflow$conflicts, FALSE)
  expect_identical(m_wflow$sha, NA_character_)
  expect_identical(
    status(r)$untracked,
    list(untracked = "test.txt")
  )

  m <- merge(r, "feature", fail = FALSE)
  expect_true(inherits(m, "git_merge_result"))
  expect_identical(m$up_to_date, FALSE)
  expect_identical(m$fast_forward, NULL)
  expect_identical(m$conflicts, NULL)
  expect_identical(m$sha, NULL)
  expect_identical(
    status(r)$untracked,
    list(untracked = "test.txt")
  )
})

# fail=TRUE vs. fail=FALSE gives different results in this context
test_that("staged and commited conflicting changes", {
  path <- test_setup()
  on.exit(test_teardown(path))

  r <- repository(path)
  f <- file.path(path, "test.txt")
  checkout(r, branch = "feature", create = TRUE)
  writeLines("feature branch", con = f)
  add(r, f)
  commit(r, "commit on feature branch")
  checkout(r, "master")
  writeLines("master branch", con = f)
  add(r, f)
  commit(r, "commit on master branch")
  writeLines("staged changes", con = f)
  add(r, f)
  expect_identical(
    status(r)$staged,
    list(modified = "test.txt")
  )

  m_fail <- merge(r, "feature", fail = TRUE)
  expect_true(inherits(m_fail, "git_merge_result"))
  expect_identical(m_fail$up_to_date, FALSE)
  expect_identical(m_fail$fast_forward, FALSE)
  expect_identical(m_fail$conflicts, TRUE)
  expect_identical(m_fail$sha, NA_character_)
  expect_identical(
    status(r)$staged,
    list(modified = "test.txt")
  )

  m_wflow <- workflowr:::git2r_merge(r, "feature", fail = TRUE)
  expect_true(inherits(m_wflow, "git_merge_result"))
  expect_identical(m_wflow$up_to_date, FALSE)
  expect_identical(m_wflow$fast_forward, FALSE)
  expect_identical(m_wflow$conflicts, TRUE)
  expect_identical(m_wflow$sha, NA_character_)
  expect_identical(
    status(r)$staged,
    list(modified = "test.txt")
  )

  m <- merge(r, "feature", fail = FALSE)
  expect_true(inherits(m, "git_merge_result"))
  expect_identical(m$up_to_date, FALSE)
  expect_identical(m$fast_forward, FALSE)
  expect_identical(m$conflicts, NULL)
  expect_identical(m$sha, NULL)
  expect_identical(
    status(r)$staged,
    list(modified = "test.txt")
  )
})
