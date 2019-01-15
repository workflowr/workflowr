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
  undo <- wflow_rename(new, original, message = "Revert to original name")
  expect_true(fs::file_exists(original))
  expect_false(fs::file_exists(new))
  expect_identical(undo$files_git, c(new, original))
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


# wflow_rename checks that all files exists with fs::file_exists(). On Linux and
# macOS this is fine because both fs::file_exists("dir") and fs::file_exists("dir/")
# return TRUE. However, on Windows this is not the case: fs::file_exists("dir")
# returns TRUE and fs::file_exists("dir/") returns FALSE.
test_that("wflow_rename can handle a trailing slash in a directory name", {
  original <- "code/"
  new <- "scripts/"
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
  file.create(fig)
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
