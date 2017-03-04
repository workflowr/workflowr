context("create_gitignore")

# Create a temp directory
tmp_dir <- tempfile("test-git-")
dir.create(tmp_dir)

# Create a .gitignore file
workflowr:::create_gitignore(tmp_dir)
fname_expected <- file.path(tmp_dir, ".gitignore")

test_that(".gitignore file was created", {
  expect_true(file.exists(fname_expected))
})

test_that(".gitignore is not overwritten by default", {
  mod_time_pre <- file.mtime(fname_expected)
  expect_warning(workflowr:::create_gitignore(tmp_dir))
  mod_time_post <- file.mtime(fname_expected)
  expect_true(mod_time_post == mod_time_pre)
})

test_that(".gitignore is overwritten if forced", {
  mod_time_pre <- file.mtime(fname_expected)
  expect_silent(workflowr:::create_gitignore(tmp_dir, overwrite = TRUE))
  mod_time_post <- file.mtime(fname_expected)
  expect_true(mod_time_post > mod_time_pre)
})

# Delete temp directory
unlink(tmp_dir)
