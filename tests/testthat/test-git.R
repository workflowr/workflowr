context("git")

# Setup ------------------------------------------------------------------------

# Load helper function local_no_gitconfig()
source("helpers.R", local = TRUE)

source("setup.R", local = TRUE)

skip_on_cran_windows()

# Test get_committed_files and obtain_files_in_commit --------------------------

# Create temp Git directory
dir_git <- tempfile("test-get_committed_files-")
fs::dir_create(dir_git)
dir_git <- workflowr:::absolute(dir_git)
on.exit(unlink(dir_git, recursive = TRUE, force = TRUE))
# Initialize Git repo
git2r::init(dir_git)
r <- git2r::repository(dir_git)
git2r::config(r, user.name = "Test Name", user.email = "test@email")

test_that("get_committed_files returns NA if no files have been committed", {
  expect_identical(workflowr:::get_committed_files(r), NA)
  expect_identical(workflowr:::get_committed_files(r, sysgit = ""), NA)
})

# Commit some files in root commit
f <- file.path(dir_git, c("a.txt", "b.txt"))
fs::file_create(f)
workflowr:::git2r_add(r, f)
git2r::commit(r, message = "root commit")

test_that("get_committed_files works on root commit", {
  expected <- f
  expect_identical(workflowr:::get_committed_files(r), expected)
  expect_identical(workflowr:::get_committed_files(r, sysgit = ""), expected)
})

test_that("obtain_files_in_commit works on root commit", {
  expected <- f
  actual <- workflowr:::obtain_files_in_commit(r, git2r::commits(r)[[1]])
  expect_identical(actual, expected)
})

# Commit more files
f2 <- file.path(dir_git, c("c.txt", "d.txt"))
fs::file_create(f2)
workflowr:::git2r_add(r, f2)
git2r::commit(r, message = "another commit")

test_that("get_committed_files works on multiple commits", {
  expected <- c(f, f2)
  expect_identical(workflowr:::get_committed_files(r), expected)
  expect_identical(workflowr:::get_committed_files(r, sysgit = ""), expected)
})

test_that("obtain_files_in_commit works on standard commit", {
  expected <- f2
  actual <- workflowr:::obtain_files_in_commit(r, git2r::commits(r)[[1]])
  expect_identical(actual, expected)
})

test_that("get_committed_files can be passed a specific commit", {
  expected <- f
  expect_identical(
    workflowr:::get_committed_files(r, commit =  git2r::commits(r)[[2]]),
    expected)
  # Note: Testing with `sysgit = ""` is unnecessary here b/c libgit2 is always
  # used to retreive the files present at a given commit.
})

# Remove a file
git2r::rm_file(r, basename(f[1]))
git2r::commit(r, message = "remove a file")

test_that("get_committed_files stops reporting files after they are removed", {
  expected <- c(f[2], f2)
  expect_identical(workflowr:::get_committed_files(r), expected)
  expect_identical(workflowr:::get_committed_files(r, sysgit = ""), expected)
})

test_that("obtain_files_in_commit reports a deleted file", {
  expected <- f[1]
  actual <- workflowr:::obtain_files_in_commit(r, git2r::commits(r)[[1]])
  expect_identical(actual, expected)
})

test_that("get_committed_files can handle a path with spaces", {

  path <- test_setup(path = fs::file_temp(" a path with spaces "))
  on.exit(test_teardown(path))
  r <- git2r::repository(path)

  readme <- file.path(path, "README.md")

  expect_true(readme %in% workflowr:::get_committed_files(r))
  expect_true(readme %in% workflowr:::get_committed_files(r, sysgit = ""))
})

# Test check_git_config --------------------------------------------------------

test_that("check_git_config throws an error when user.name and user.email are not set", {

  skip_on_cran()

  # local_no_gitconfig() is defined in tests/testthat/helpers.R
  local_no_gitconfig("-workflowr")

  expect_error(workflowr:::check_git_config("."),
               "You must set your user.name and user.email for Git first")

  custom_message <- "fname"
  expect_error(workflowr:::check_git_config(".", custom_message = custom_message),
               custom_message)
})

# Test check_git_lock ----------------------------------------------------------

test_that("check_git_lock throws error only when Git repository is locked", {

  path <- test_setup()
  on.exit(test_teardown(path))
  r <- git2r::repository(path)

  expect_silent(workflowr:::check_git_lock(r))
  index_lock <- file.path(git2r::workdir(r), ".git/index.lock")
  fs::file_create(index_lock)
  expect_error(workflowr:::check_git_lock(r), "The Git repository is locked")
  expect_error(workflowr:::check_git_lock(r), index_lock)
  fs::file_delete(index_lock)
  expect_silent(workflowr:::check_git_lock(r))
})

# Test commits(path) -----------------------------------------------------------

test_that("commits(path) returns commits in reverse chronological order", {

  # Uses the workflowr git log for testing
  if (!git2r::in_repository()) skip("Requires the workflowr Git repository")

  r <- git2r::repository()

  first_commit <- git2r::commits(r, reverse = TRUE, n = 1)
  if (length(first_commit) == 0) skip("Requires the workflowr Git repository")
  first_sha <- first_commit[[1]]$sha
  if (!first_sha == "dc396e923537ced01babc9f893e691dababee89b")
    skip("Requires the workflowr Git repository")

  files <- c("README.md", "vignettes/wflow-07-common-code.Rmd")
  files <- file.path(git2r::workdir(r), files)

  for (f in files) {
    commits_path <- git2r::commits(r, path = f)
    date <- vapply(commits_path, function(x) as.character(x$author$when),
                   character(1))
    date <- as.POSIXct(date)

    expect_true(all(date[seq_len(length(date) - 1)] >= date[seq(2, length(date))]))
  }

})

# Test get_outdated_files ------------------------------------------------------

test_that("Git and git2r return same unix time for last commit", {

  sysgit <- getOption("workflowr.sysgit", default = "")
  if (is.null(sysgit) || is.na(sysgit) || nchar(sysgit) == 0)
    skip("No system Git available")

  path <- test_setup()
  on.exit(test_teardown(path))
  r <- git2r::repository(path)
  readme <- workflowr:::absolute(file.path(path, "README.md"))

  result_last_commit_time_sysgit <- workflowr:::last_commit_time_sysgit(r, readme, sysgit = sysgit)
  result_last_commit_time_git2r <- workflowr:::last_commit_time_git2r(r, readme)

  expect_identical(
    result_last_commit_time_sysgit,
    result_last_commit_time_git2r
  )

  expect_true(is.numeric(result_last_commit_time_sysgit))
})

test_that("Git and git2r return same outdated files", {

  sysgit <- getOption("workflowr.sysgit", default = "")
  if (is.null(sysgit) || is.na(sysgit) || nchar(sysgit) == 0)
    skip("No system Git available")

  path <- test_setup()
  on.exit(test_teardown(path))
  r <- git2r::repository(path)
  p <- workflowr:::wflow_paths(project = path)
  # Use absolute paths. For some reason git2r sometimes fails with relative
  # paths, e.g. on winbuilder. Impossible to debug.
  p <- lapply(p, workflowr:::absolute)
  rmd <- list.files(path = p$analysis, pattern = "Rmd$", full.names = TRUE)
  html <- workflowr:::to_html(rmd, outdir = p$docs)

  # Fake publish them by committing HTML
  Sys.sleep(1.25)
  fs::file_create(html)
  git2r::add(r, html)
  git2r::commit(r, "Publish HTML files")
  # Make the first one outdated
  Sys.sleep(1.25)
  cat("edit\n", file = rmd[1], append = TRUE)
  git2r::add(r, rmd[1])
  git2r::commit(r, "Make outdated Rmd")

  outdated_sysgit <- workflowr:::get_outdated_files(r,
                                                    files = rmd,
                                                    outdir = p$docs,
                                                    sysgit = sysgit)

  outdated_git2r <- workflowr:::get_outdated_files(r,
                                                   files = rmd,
                                                   outdir = p$docs,
                                                   sysgit = "")

  expect_identical(
    outdated_sysgit,
    outdated_git2r
  )

  expect_identical(
    outdated_sysgit,
    rmd[1]
  )
})
