context("wflow_rename")

# Setup ------------------------------------------------------------------------

library("git2r")

cwd <- getwd()
tdir <- tempfile("test-wflow_rename-")
on.exit(setwd(cwd))
on.exit(unlink(tdir, recursive = TRUE, force = TRUE), add = TRUE)
suppressMessages(wflow_start(tdir, user.name = "Test Name",
                             user.email = "test@email"))
tdir <- workflowr:::absolute(tdir)
r <- repository(path = tdir)
p <- wflow_paths()

# Test wflow_rename ------------------------------------------------------------

test_that("wflow_rename can rename one file", {
  original <- "analysis/about.Rmd"
  new <- "analysis/new.Rmd"

  renamed <- wflow_rename(original, new, message = "Rename file")
  expect_false(fs::file_exists(original))
  expect_true(fs::file_exists(new))
  expect_identical(renamed$files_git, c(original, new))
  last_commit <- commits(r, n = 1)[[1]]
  last_commit_mes <- workflowr:::git2r_slot(last_commit, "message")
  last_commit_sha <- workflowr:::git2r_slot(last_commit, "sha")
  expect_identical(last_commit_mes, "Rename file")
  # Test print method
  expect_output(print(renamed), sprintf("%s -> %s", original, new))
  expect_output(print(renamed),
                stringr::str_sub(last_commit_sha, start = 1, end = 7))
  expect_output(print(renamed), "commit message:")

  undo <- wflow_rename(new, original, message = "Revert to original name")
  expect_true(fs::file_exists(original))
  expect_false(fs::file_exists(new))
  expect_identical(undo$files_git, c(new, original))
  last_commit <- commits(r, n = 1)[[1]]
  last_commit_mes <- workflowr:::git2r_slot(last_commit, "message")
  last_commit_sha <- workflowr:::git2r_slot(last_commit, "sha")
  expect_identical(last_commit_mes, "Revert to original name")
  # Test print method
  expect_output(print(undo), sprintf("%s -> %s", new, original))
  expect_output(print(undo),
                stringr::str_sub(last_commit_sha, start = 1, end = 7))
  expect_output(print(renamed), "commit message:")
})

test_that("wflow_rename can rename one file when no Git repo", {
  git <- ".git"
  git_tmp <- ".git-tmp"
  on.exit(fs::file_move(git_tmp, git))
  fs::file_move(git, git_tmp)

  original <- "analysis/about.Rmd"
  new <- "analysis/new.Rmd"

  renamed <- wflow_rename(original, new, message = "Rename file")
  expect_false(fs::file_exists(original))
  expect_true(fs::file_exists(new))
  expect_identical(renamed$files, original, new)
  expect_identical(renamed$files_git, NA_character_)

  undo <- wflow_rename(new, original, message = "Revert to original name")
  expect_true(fs::file_exists(original))
  expect_false(fs::file_exists(new))
  expect_identical(undo$files, new)
  expect_identical(undo$files_git, NA_character_)

  # Test print method
  print_lines <- utils::capture.output(print(undo))
  expect_true(sprintf("%s -> %s", new, original) %in% print_lines)
  expect_false("commit message:" %in% print_lines)
})

test_that("wflow_rename can rename one directory", {
  original <- "code"
  new <- "scripts"

  renamed <- wflow_rename(original, new, message = "Rename code directory")
  expect_false(fs::dir_exists(original))
  expect_true(fs::dir_exists(new))
  expect_identical(renamed$files_git,
                   c(file.path(original, "README.md"),
                     file.path(new, "README.md")))

  undo <- wflow_rename(new, original,
                       message = "Revert to original directory name")
  expect_true(fs::dir_exists(original))
  expect_false(fs::dir_exists(new))
  expect_identical(undo$files_git,
                   c(file.path(new, "README.md"),
                     file.path(original, "README.md")))
})

test_that("wflow_rename can rename one directory from outside project", {
  # Temporarily move out of the project directory, using the variables defined
  # in Setup above
  setwd(cwd)
  on.exit(setwd(tdir))

  original <- file.path(tdir, "code")
  new <- file.path(tdir, "scripts")

  renamed <- wflow_rename(original, new, message = "Rename code directory",
                          project = tdir)
  expect_false(fs::dir_exists(original))
  expect_true(fs::dir_exists(new))
  expect_identical(renamed$files_git,
                   workflowr:::relative(c(file.path(original, "README.md"),
                              file.path(new, "README.md"))))

  undo <- wflow_rename(new, original,
                       message = "Revert to original directory name",
                       project = tdir)
  expect_true(fs::dir_exists(original))
  expect_false(fs::dir_exists(new))
  expect_identical(undo$files_git,
                   workflowr:::relative(c(file.path(new, "README.md"),
                                          file.path(original, "README.md"))))
})

# In the past I've had some issues with fs::file_exists()/fs::dir_exists() and
# trailing slashes in filepaths. I no longer observe this issue, and this test
# will catch it if it comes back.
test_that("wflow_rename can handle a trailing slash in a directory name", {
  original <- "code/"
  new <- "scripts/"

  expect_identical(workflowr:::absolute(getwd()), tdir)
  expect_true(fs::dir_exists(original),
              info = glue::glue("wd is {fs::path_wd()} and contains \n{paste(fs::dir_ls(), collapse = '\n')}"))

  renamed <- wflow_rename(original, new, message = "Rename code directory")
  expect_false(fs::dir_exists(original))
  expect_true(fs::dir_exists(new))
  expect_identical(renamed$files_git,
                   workflowr:::relative(c(file.path(original, "README.md"),
                                          file.path(new, "README.md"))))

  undo <- wflow_rename(new, original,
                       message = "Revert to original directory name")
  expect_true(fs::dir_exists(original))
  expect_false(fs::dir_exists(new))
  expect_identical(undo$files_git,
                   workflowr:::relative(c(file.path(new, "README.md"),
                                          file.path(original, "README.md"))))
})

test_that("wflow_rename can rename one Rmd file with HTML and figs", {

  skip_on_cran()

  original <- "analysis/about.Rmd"
  new <- "analysis/new.Rmd"
  wflow_publish(original, view = FALSE)
  # Add a fake fig and commit it
  dir_fig <- file.path(p$docs, workflowr:::create_figure_path(original))
  fs::dir_create(dir_fig)
  fig <- file.path(dir_fig, "fig.png")
  fs::file_create(fig)
  add(r, fig)
  commit(r, "Publish fake fig to accompany Rmd file to be renamed")
  # Expected files
  html <- workflowr:::to_html(original, outdir = p$docs)
  html_new <- workflowr:::to_html(new, outdir = p$docs)
  dir_fig_new <- file.path(p$docs, workflowr:::create_figure_path(new))
  fig_new <- file.path(dir_fig_new, "fig.png")

  renamed <- wflow_rename(original, new, message = "Rename file")

  expect_false(fs::file_exists(original))
  expect_true(fs::file_exists(new))
  expect_false(fs::file_exists(html))
  expect_true(fs::file_exists(html_new))
  expect_false(fs::file_exists(fig))
  expect_true(fs::file_exists(fig_new))
  expect_identical(renamed$files_git, c(original, html, fig,
                                        new, html_new, fig_new))

  undo <- wflow_rename(new, original, message = "Revert to original name")

  expect_true(fs::file_exists(original))
  expect_false(fs::file_exists(new))
  expect_true(fs::file_exists(html))
  expect_false(fs::file_exists(html_new))
  expect_true(fs::file_exists(fig))
  expect_false(fs::file_exists(fig_new))
  expect_identical(undo$files_git, c(new, html_new, fig_new,
                                     original, html, fig))
})

# Test print.wflow_rename ------------------------------------------------------

test_that("print.wflow_rename works with dry_run", {
  original <- "README.md"
  new <- "new.md"

  renamed <- wflow_rename(original, new, dry_run = TRUE)
  expect_output(print(renamed), "The following file\\(s\\) would be renamed:")
  expect_output(print(renamed), sprintf("%s -> %s", original, new))
  expect_output(print(renamed),
                "The following file\\(s\\) would be included in the Git commit:")
})

test_that("print.wflow_rename works with `git = FALSE`", {
  original <- "README.md"
  new <- "new.md"

  git_true <- wflow_rename(original, new, git = TRUE, dry_run = TRUE)
  git_true_print_lines <- utils::capture.output(print(git_true))
  expect_true("commit message:" %in% git_true_print_lines)

  git_false <- wflow_rename(original, new, git = FALSE, dry_run = TRUE)
  git_false_print_lines <- utils::capture.output(print(git_false))
  expect_false("commit message:" %in% git_false_print_lines)
})
