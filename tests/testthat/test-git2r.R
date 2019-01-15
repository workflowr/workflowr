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
b1 <- git2r_head(r)

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
  expect_identical(git2r_slot(b2, "name"), "b2")
})

test_that("merge returns git_merge_result", {
  m1 <- git2r_merge(r, "b2")
  expect_equivalent(class(m1), "git_merge_result")
  expect_identical(git2r_slot(m1, "up_to_date"), FALSE)
  expect_identical(git2r_slot(m1, "fast_forward"), TRUE)
  expect_identical(git2r_slot(m1, "conflicts"), FALSE)
  expect_identical(git2r_slot(m1, "sha"), NA_character_)

  # Merge a second time, which has no effect
  m2 <- git2r_merge(r, "b2")
  expect_equivalent(class(m2), "git_merge_result")
  expect_identical(git2r_slot(m2, "up_to_date"), TRUE)
  expect_identical(git2r_slot(m2, "fast_forward"), FALSE)
  expect_identical(git2r_slot(m2, "conflicts"), FALSE)
  expect_identical(git2r_slot(m2, "sha"), NA_character_)

  # Merge parallel branch, which should create a merge commit
  m3 <- git2r_merge(r, "b3")
  expect_equivalent(class(m3), "git_merge_result")
  expect_identical(git2r_slot(m3, "up_to_date"), FALSE)
  expect_identical(git2r_slot(m3, "fast_forward"), FALSE)
  expect_identical(git2r_slot(m3, "conflicts"), FALSE)
  expect_identical(git2r_slot(m3, "sha"),
                   git2r_slot(commits(r)[[1]], "sha"))

  # Merge conflicting branch, which should create a merge conflict
  m4 <- git2r_merge(r, "b4")
  expect_equivalent(class(m4), "git_merge_result")
  expect_identical(git2r_slot(m4, "up_to_date"), FALSE)
  expect_identical(git2r_slot(m4, "fast_forward"), FALSE)
  expect_identical(git2r_slot(m4, "conflicts"), TRUE)
  expect_identical(git2r_slot(m4, "sha"), NA_character_)
})
