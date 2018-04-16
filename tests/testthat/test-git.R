context("git")

# Test get_committed_files -----------------------------------------------------

# Create temp Git directory
dir_git <- tempfile("test-get_committed_files-")
dir.create(dir_git)
dir_git <- workflowr:::absolute(dir_git)
on.exit(unlink(dir_git, recursive = TRUE, force = TRUE))
# Initialize Git repo
git2r::init(dir_git)
r <- git2r::repository(dir_git)
git2r::config(r, user.name = "Test Name", user.email = "test@email")

test_that("get_committed_files fails if no files have been committed", {
  expect_error(actual <- workflowr:::get_committed_files(r),
               "The Git repository has no commits yet.")
})

# Commit some files in root commit
f <- file.path(dir_git, c("a.txt", "b.txt"))
file.create(f)
git2r::add(r, f)
git2r::commit(r, message = "root commit")

test_that("get_committed_files works on root commit", {
  expected <- basename(f)
  actual <- workflowr:::get_committed_files(r)
  expect_identical(actual, expected)
})

# Commit more files
f2 <- file.path(dir_git, c("c.txt", "d.txt"))
file.create(f2)
git2r::add(r, f2)
git2r::commit(r, message = "another commit")

test_that("get_committed_files works on multiple commits", {
  expected <- basename(c(f, f2))
  actual <- workflowr:::get_committed_files(r)
  expect_identical(actual, expected)
})

# Remove a file
git2r::rm_file(r, basename(f[1]))
git2r::commit(r, message = "remove a file")

test_that("get_committed_files stops reporting files after they are removed", {
  expected <- basename(c(f[2], f2))
  actual <- workflowr:::get_committed_files(r)
  expect_identical(actual, expected)
})
