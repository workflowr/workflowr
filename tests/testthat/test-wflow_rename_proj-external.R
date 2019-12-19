context("wflow_rename_proj-external")

# Test wflow_rename_proj() when run from outside a workflowr project.

# Setup ------------------------------------------------------------------------

source("setup.R")

skip_on_cran_windows()

path <- test_setup()
wd <- absolute(getwd())

wflow_use_gitlab("user", "repo", project = path)
newname <- "new"
path_new <- wflow_rename_proj(newname, project = path)
on.exit(test_teardown(path_new), add = TRUE)
s <- wflow_status(project = path_new)

# Test wflow_rename_proj() (from external directory) ---------------------------

test_that("wflow_rename_proj() does not change the working directory if run from outside project", {
  expect_identical(wd, absolute(getwd()))
})

test_that("wflow_rename_proj() renames RStudio Project file", {
  rproj <- file.path(s$root, paste0(newname, ".Rproj"))
  expect_true(fs::file_exists(rproj))
})

test_that("wflow_rename_proj() updates remote URL", {
  remote_avail <- wflow_git_remote(project = path_new)
  origin <- remote_avail["origin"]
  names(origin) <- NULL
  expected <- paste0("https://gitlab.com/user/new.git")
  expect_identical(origin, expected)
})

test_that("wflow_rename_proj() renames navbar title", {
  site_yml_fname <- file.path(s$analysis, "_site.yml")
  site_yml <- yaml::yaml.load_file(site_yml_fname)
  expect_identical(site_yml$name, newname)
  expect_identical(site_yml$navbar$title, newname)
})

test_that("wflow_rename_proj() renames README title", {
  readme_fname <- file.path(s$root, "README.md")
  readme_title_expected <- paste("#", newname)
  readme_lines <- readLines(readme_fname)
  readme_title_observed <- readme_lines[1]
  expect_identical(readme_title_observed, readme_title_expected)
})

test_that("wflow_rename_proj() commits changes", {
  r <- git2r::repository(path = path_new)
  commit <- git2r::commits(r, n = 1)[[1]]
  commit_message <- commit$message
  expect_identical(commit_message, paste("Rename project to", newname))
})

test_that("wflow_rename_proj() renames project directory", {
  expect_identical(basename(path_new), newname)
})

test_that("wflow_rename_proj() can be re-run without error", {
  expect_message(wflow_rename_proj(newname, project = path_new),
                 "RStudio Project file already named")

  expect_message(wflow_rename_proj(newname, project = path_new),
                 "No changes to commit")

  expect_message(wflow_rename_proj(newname, project = path_new),
                 "Project directory already named:")
})
