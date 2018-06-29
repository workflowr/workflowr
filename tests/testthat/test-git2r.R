context("git2r")

# Setup ------------------------------------------------------------------------

library("git2r")
x <- tempfile()
on.exit(unlink(x, recursive = TRUE, force = TRUE))
dir.create(x)
r <- init(x)
f1 <- file.path(x, "f1.txt")
file.create(f1)
add(r, f1)
c1 <- commit(r, "commit 1")
t1 <- tree(c1)
b1 <- branch_create(c1, name = "b1")
checkout(b1)
f2 <- file.path(x, "f2.txt")
file.create(f2)
add(r, f2)
c2 <- commit(r, "commit 2")
t2 <- tree(c2)

# Test git2r -------------------------------------------------------------------

test_that("head returns a branch", {
  h <- git2r_head(r)
  expect_equivalent(class(h), "git_branch")
})

test_that("lookup returns a commit", {
  c_look <- lookup(r, git2r_slot(c1, "sha"))
  expect_equivalent(class(c_look), "git_commit")
  expect_identical(c_look, c1)
})

test_that("workdir does not includes a trailing slash", {
  wd <- git2r_workdir(r)
  expect_false(stringr::str_sub(wd, start = -1) == "/")
})

test_that("as.list returns a list", {
  l <- git2r_as.list(t1)
  expect_equivalent(class(l), "list")
})

test_that("as.data.frame returns a data.frame", {
  d <- git2r_as.data.frame(t1)
  expect_equivalent(class(d), "data.frame")
})

test_that("diff returns a diff", {
  difference <- git2r_diff(t1, t2)
  expect_equivalent(class(difference), "git_diff")
  diff_file_list <- git2r_slot(difference, "files")
  expect_equivalent(class(diff_file_list), "list")
  diff_file <- diff_file_list[[1]]
  expect_equivalent(class(diff_file), "git_diff_file")
  new_file <- git2r_slot(diff_file, "new_file")
  expect_equivalent(class(new_file), "character")
})

test_that("slot returns slot values", {
  expect_identical(git2r_slot(c1, "message"), "commit 1")
  expect_identical(class(git2r_slot(c1, "sha")), "character")
  expect_identical(git2r_slot(b1, "name"), "b1")
})

test_that("merge returns git_merge_result", {
  m <- git2r_merge(r, "b1")
  expect_equivalent(class(m), "git_merge_result")
})
