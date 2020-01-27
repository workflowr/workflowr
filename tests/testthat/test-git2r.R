context("git2r")

# Setup ------------------------------------------------------------------------

library("git2r")
x <- tempfile()
on.exit(unlink(x, recursive = TRUE, force = TRUE))
fs::dir_create(x)
r <- init(x)
config(r, user.name = "Test Name", user.email = "test@email")

f1 <- file.path(x, "f1.txt")
fs::file_create(f1)
add(r, f1)
c1 <- commit(r, "commit 1")
t1 <- tree(c1)
b1 <- git2r::repository_head(r)

b2 <- branch_create(c1, name = "b2")
checkout(b2)
f2 <- file.path(x, "f2.txt")
writeLines("original\n", con = f2)
add(r, f2)
c2 <- commit(r, "commit 2")
t2 <- tree(c2)

b3 <- branch_create(c1, name = "b3")
checkout(b3)
f3 <- file.path(x, "f3.txt")
fs::file_create(f3)
add(r, f3)
c3 <- commit(r, "commit 3")
t3 <- tree(c3)

# Conflict branch
b4 <- branch_create(c1, name = "b4")
checkout(b4)
writeLines("replacement\n", con = f2)
add(r, f2)
c4 <- commit(r, "commit 4")
t4 <- tree(c4)

checkout(r, "master")

# Test git2r -------------------------------------------------------------------

test_that("head returns a branch", {
  h <- git2r::repository_head(r)
  expect_true(inherits(h, "git_branch"))
})

test_that("lookup returns a commit", {
  c_look <- lookup(r, c1$sha)
  expect_true(inherits(c_look, "git_commit"))
  expect_identical(c_look, c1)
})

test_that("workdir does not includes a trailing slash", {
  wd <- git2r::workdir(r)
  expect_false(stringr::str_sub(wd, start = -1) == "/")
})

test_that("as.list returns a list", {
  l <- as.list(t1)
  expect_true(inherits(l, "list"))
})

test_that("as.data.frame returns a data.frame", {
  d <- as.data.frame(t1)
  expect_true(inherits(d, "data.frame"))
})

test_that("diff returns a diff", {
  difference <- base::diff(t1, t2)
  expect_true(inherits(difference, "git_diff"))
  diff_file_list <- difference$files
  expect_true(inherits(diff_file_list, "list"))
  diff_file <- diff_file_list[[1]]
  expect_true(inherits(diff_file, "git_diff_file"))
  new_file <- diff_file$new_file
  expect_true(inherits(new_file, "character"))
})

test_that("can access object elements using $", {
  expect_identical(c1$message, "commit 1")
  expect_true(inherits(c1$sha, "character"))
  expect_identical(b2$name, "b2")
})

test_that("merge returns git_merge_result", {
  m1 <- git2r_merge(r, "b2")
  expect_true(inherits(m1, "git_merge_result"))
  expect_identical(m1$up_to_date, FALSE)
  expect_identical(m1$fast_forward, TRUE)
  expect_identical(m1$conflicts, FALSE)
  expect_identical(m1$sha, NA_character_)

  # Merge a second time, which has no effect
  m2 <- git2r_merge(r, "b2")
  expect_true(inherits(m2, "git_merge_result"))
  expect_identical(m2$up_to_date, TRUE)
  expect_identical(m2$fast_forward, FALSE)
  expect_identical(m2$conflicts, FALSE)
  expect_identical(m2$sha, NA_character_)

  # Merge parallel branch, which should create a merge commit
  m3 <- git2r_merge(r, "b3")
  expect_true(inherits(m3, "git_merge_result"))
  expect_identical(m3$up_to_date, FALSE)
  expect_identical(m3$fast_forward, FALSE)
  expect_identical(m3$conflicts, FALSE)
  expect_identical(m3$sha,
                   commits(r)[[1]]$sha)

  # Merge conflicting branch, which should create a merge conflict
  # fail = TRUE
  m4 <- git2r_merge(r, "b4", fail = TRUE)
  expect_true(inherits(m4, "git_merge_result"))
  expect_identical(m4$up_to_date, FALSE)
  expect_identical(m4$fast_forward, FALSE)
  expect_identical(m4$conflicts, TRUE)
  expect_identical(m4$sha, NA_character_)
  # working directory should be clean b/c fail=TRUE
  expect_identical(status(r)$unstaged, structure(list(), .Names = character(0)))
  # fail = FALSE
  m5 <- git2r_merge(r, "b4", fail = FALSE)
  expect_true(inherits(m5, "git_merge_result"))
  expect_identical(m5$up_to_date, FALSE)
  expect_identical(m5$fast_forward, FALSE)
  expect_identical(m5$conflicts, TRUE)
  expect_identical(m5$sha, NA_character_)
  # working directory should have merge conflicts as unstaged changes
  expect_identical(status(r)$unstaged, list(conflicted = "f2.txt"))
})
