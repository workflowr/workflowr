context("wflow_status")

# Setup ------------------------------------------------------------------------

source("setup.R")

skip_on_cran_windows()

# Setup workflowr project for testing
site_dir <- tempfile("test-wflow_status-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE, user.name = "Test Name",
                             user.email = "test@email"))
site_dir <- workflowr:::relative(site_dir)
# Cleanup
on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))

# Note: Only a few tests in this file use the modern setup
source("setup.R")

# Test wflow_status ------------------------------------------------------------

s <- wflow_status(project = site_dir)

test_that("wflow_status identifies root directory.", {
  expected <- site_dir
  actual <- s$root
  expect_identical(actual, expected)
  expect_true(fs::dir_exists(expected))
})

test_that("wflow_status identifies analysis directory.", {
  expected <- file.path(site_dir, "analysis")
  actual <- s$analysis
  expect_identical(actual, expected)
  expect_true(fs::dir_exists(expected))
})

test_that("wflow_status identifies docs directory.", {
  expected <- file.path(site_dir, "docs")
  actual <- s$docs
  expect_identical(actual, expected)
  expect_true(fs::dir_exists(expected))
})

test_that("wflow_status identifies Git directory.", {
  expected <- site_dir
  actual <- s$git
  expect_identical(actual, expected)
  expect_true(fs::dir_exists(expected))
})

test_that("wflow_status returns data frame of logical values on Rmd files.", {
  expect_is(s$status, "data.frame")
  for (column in colnames(s$status)) {
    expect_is(s$status[, column], "logical")
  }
  expect_is(rownames(s$status), "character")
  expect_identical(colnames(s$status),
                   c("ignored", "mod_unstaged", "conflicted", "mod_staged",
                     "tracked", "committed", "published", "mod_committed",
                     "modified", "unpublished", "scratch"))
})

# Skip on CRAN. See ?testthat::skip_on_cran, which only works inside of unit
# test functions.
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  # index.Rmd will have status published
  rmd_pub <- file.path(s$analysis, "index.Rmd")
  # about.Rmd will have status modified
  rmd_mod <- file.path(s$analysis, "about.Rmd")
  # license.Rmd will have status Unp for Unpublished
  rmd_unp <- file.path(s$analysis, "license.Rmd")
  # Create a new untracked file that will have status Scr for Scratch
  rmd_scr <- file.path(s$analysis, "scratch.Rmd")
  fs::file_create(rmd_scr)
  # Create a new file that will be published, modified, and then only committed
  rmd_mod_committed <- file.path(s$analysis, "mod-committed.Rmd")
  fs::file_create(rmd_mod_committed)

  # Publish index.Rmd, about.Rmd, and mod-committed.Rmd
  suppressMessages(wflow_publish(c(rmd_pub, rmd_mod, rmd_mod_committed),
                                 view = FALSE, project = site_dir))
  # Modify about.Rmd to have status Mod for Modified
  cat("edit\n", file = rmd_mod, append = TRUE)
  # Modify mod-committed.Rmd and then commit the change
  cat("edit\n", file = rmd_mod_committed, append = TRUE)
  wflow_git_commit(rmd_mod_committed, "Commit but don't publish",
                   project = site_dir)
}

test_that("wflow_status classifies files when run from outside workflowr project.", {

  skip_on_cran()

  s_tmp <- wflow_status(project = site_dir)
  expect_true(s_tmp$status[rmd_scr, "scratch"])
  expect_true(s_tmp$status[rmd_pub, "published"])
  expect_true(s_tmp$status[rmd_mod, "modified"])
  expect_true(s_tmp$status[rmd_unp, "unpublished"])
  expect_true(s_tmp$status[rmd_mod_committed, "mod_committed"])
})

test_that("wflow_status classifies files when run from root of workflowr project.", {

  skip_on_cran()

  rmd_scr <- workflowr:::relative(rmd_scr, start = s$root)
  rmd_pub <- workflowr:::relative(rmd_pub, start = s$root)
  rmd_mod <- workflowr:::relative(rmd_mod, start = s$root)
  rmd_unp <- workflowr:::relative(rmd_unp, start = s$root)
  rmd_mod_committed <- workflowr:::relative(rmd_mod_committed, start = s$root)

  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(s$root)
  s_tmp <- wflow_status()
  expect_true(s_tmp$status[rmd_scr, "scratch"])
  expect_true(s_tmp$status[rmd_pub, "published"])
  expect_true(s_tmp$status[rmd_mod, "modified"])
  expect_true(s_tmp$status[rmd_unp, "unpublished"])
  expect_true(s_tmp$status[rmd_mod_committed, "mod_committed"])
})

test_that("wflow_status classifies files when run from analysis/.", {

  skip_on_cran()

  rmd_scr <- workflowr:::relative(rmd_scr, start = s$analysis)
  rmd_pub <- workflowr:::relative(rmd_pub, start = s$analysis)
  rmd_mod <- workflowr:::relative(rmd_mod, start = s$analysis)
  rmd_unp <- workflowr:::relative(rmd_unp, start = s$analysis)
  rmd_mod_committed <- workflowr:::relative(rmd_mod_committed, start = s$analysis)

  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(s$analysis)
  s_tmp <- wflow_status()
  expect_true(s_tmp$status[rmd_scr, "scratch"])
  expect_true(s_tmp$status[rmd_pub, "published"])
  expect_true(s_tmp$status[rmd_mod, "modified"])
  expect_true(s_tmp$status[rmd_unp, "unpublished"])
  expect_true(s_tmp$status[rmd_mod_committed, "mod_committed"])
})

test_that("wflow_status classifies files when run from docs/.", {

  skip_on_cran()

  rmd_scr <- workflowr:::relative(rmd_scr, start = s$docs)
  rmd_pub <- workflowr:::relative(rmd_pub, start = s$docs)
  rmd_mod <- workflowr:::relative(rmd_mod, start = s$docs)
  rmd_unp <- workflowr:::relative(rmd_unp, start = s$docs)
  rmd_mod_committed <- workflowr:::relative(rmd_mod_committed, start = s$docs)

  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(s$docs)
  s_tmp <- wflow_status()
  expect_true(s_tmp$status[rmd_scr, "scratch"])
  expect_true(s_tmp$status[rmd_pub, "published"])
  expect_true(s_tmp$status[rmd_mod, "modified"])
  expect_true(s_tmp$status[rmd_unp, "unpublished"])
  expect_true(s_tmp$status[rmd_mod_committed, "mod_committed"])
})

test_that("wflow_status reports only specified files", {

  skip_on_cran()

  s_tmp <- wflow_status(rmd_scr, project = site_dir)
  expect_identical(rownames(s_tmp$status), rmd_scr)
  s_tmp <- wflow_status(c(rmd_scr, rmd_unp, rmd_pub), project = site_dir)
  expect_identical(rownames(s_tmp$status), c(rmd_scr, rmd_unp, rmd_pub))
})

test_that("wflow_status print method works", {

  skip_on_cran()

  s_tmp <- wflow_status(project = site_dir)
  s_print <- utils::capture.output(print(s_tmp))

  expect_true(paste("Scr", rmd_scr) %in% s_print)
  expect_true(paste("Mod", rmd_mod) %in% s_print)
  expect_true(paste("Mod", rmd_mod_committed) %in% s_print)
  expect_true(paste("Unp", rmd_unp) %in% s_print)
  expect_false(paste("Mod", rmd_pub) %in% s_print)
})

test_that("wflow_status detects files with extension .rmd", {
  lowercase <- file.path(s$analysis, "lowercase.rmd")
  fs::file_create(lowercase)
  on.exit(unlink(lowercase), add = TRUE)
  s_rmd <- wflow_status(lowercase, project = site_dir)
  expect_identical(rownames(s_rmd$status), lowercase)
})

test_that("wflow_status detects uncommitted changes in configuration files", {
  s_config <- wflow_status(project = site_dir)
  expect_false(s_config$site_yml)
  expect_false(s_config$wflow_yml)

  site_yml_path <- file.path(s$analysis, "_site.yml")
  site_yml_tmp <- fs::file_temp()
  fs::file_copy(site_yml_path, site_yml_tmp)
  on.exit(fs::file_move(site_yml_tmp, site_yml_path))
  site_yml <- yaml::yaml.load_file(site_yml_path)
  site_yml$key <- "value"
  yaml::write_yaml(site_yml, file = site_yml_path)

  s_config <- wflow_status(project = site_dir)
  expect_true(s_config$site_yml)
  expect_false(s_config$wflow_yml)

  wflow_yml_path <- file.path(s$root, "_workflowr.yml")
  wflow_yml_tmp <- fs::file_temp()
  fs::file_copy(wflow_yml_path, wflow_yml_tmp)
  on.exit(fs::file_move(wflow_yml_tmp, wflow_yml_path), add = TRUE)
  wflow_yml <- yaml::yaml.load_file(wflow_yml_path)
  wflow_yml$key <- "value"
  yaml::write_yaml(wflow_yml, file = wflow_yml_path)

  s_config <- wflow_status(project = site_dir)
  expect_true(s_config$site_yml)
  expect_true(s_config$wflow_yml)

  fs::file_delete(wflow_yml_path)

  s_config <- wflow_status(project = site_dir)
  expect_true(s_config$site_yml)
  expect_true(s_config$wflow_yml) # still true because "deleted" in git status
})

test_that("wflow_status works if HTML file of published Rmd is deleted", {

  skip_on_cran()

  html_pub <- workflowr:::to_html(rmd_pub, outdir = s$docs)
  html_pub_tmp <- fs::file_temp(ext = "Rmd")
  fs::file_move(html_pub, html_pub_tmp)
  on.exit(fs::file_move(html_pub_tmp, html_pub))

  expect_silent(
    status <- wflow_status(files = rmd_pub, project = site_dir)
  )
  expect_true(status$status$published)
})

# Warnings and Errors ----------------------------------------------------------

test_that("wflow_status throws error if not in workflowr project.", {
  non_project <- tempfile("non-project-")
  fs::dir_create(non_project)
  non_project <- workflowr:::absolute(non_project)
  on.exit(unlink(non_project, recursive = TRUE))
  expect_silent(s <- wflow_status(project = site_dir))
  expect_error(s <- wflow_status(project = non_project),
               "Unable to detect a workflowr project.")
})

test_that("wflow_status throws error if no RStudio .Rproj file.", {
  project_name <- basename(site_dir)
  rproj_original <- file.path(site_dir, paste0(project_name, ".Rproj"))
  rproj_replace <-  file.path(site_dir, paste0(project_name, ".txt"))
  on.exit(file.rename(rproj_replace, rproj_original))
  file.rename(rproj_original, rproj_replace)
  expect_error(s <- wflow_status(project = site_dir),
               "Unable to detect a workflowr project.")
})

test_that("wflow_status throws error if no _site.yml file.", {
  yml_original <- file.path(site_dir, "analysis/_site.yml")
  yml_replace <-  file.path(site_dir, "analysis/_site.txt")
  on.exit(file.rename(yml_replace, yml_original))
  file.rename(yml_original, yml_replace)
  expect_error(s <- wflow_status(project = site_dir),
               "Unable to find the file _site.yml in the analysis directory.")
})

test_that("wflow_status throws error if no index.Rmd file.", {
  index_original <- file.path(site_dir, "analysis/index.Rmd")
  index_replace <-  file.path(site_dir, "analysis/other.Rmd")
  on.exit(file.rename(index_replace, index_original))
  file.rename(index_original, index_replace)
  expect_error(s <- wflow_status(project = site_dir),
               "Invalid workflowr project")
})

test_that("wflow_status throws error if no Git repository.", {
  git_original <- file.path(site_dir, ".git")
  git_replace <-  file.path(site_dir, ".git2")
  on.exit(file.rename(git_replace, git_original))
  file.rename(git_original, git_replace)
  expect_error(s <- wflow_status(project = site_dir),
               "A Git repository is required for this functionality.")
})

test_that("wflow_status throws error if given directory input.", {
  d <- file.path(site_dir, "toplevel")
  fs::dir_create(d)
  on.exit(unlink(d, recursive = TRUE, force = TRUE))
  expect_error(wflow_status(d, project = site_dir),
               "files cannot include a path to a directory")
})

test_that("wflow_status throws error if given non-[Rr]md extension.", {
  readme <- file.path(site_dir, "README.md")
  expect_error(wflow_status(readme, project = site_dir),
               "Only files with extension Rmd or rmd")
})

test_that("wflow_status gives warning for HTML-only published files", {

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  rmd <- file.path(path, "analysis", "test.Rmd")
  fs::file_create(rmd)
  html <- file.path(path, "docs", "test.html")
  fs::file_create(html)

  r <- git2r::repository(path)
  git2r::add(r, html)
  git2r::commit(r, "Commit HTML only")
  expect_warning(
    wflow_status(project = path),
    workflowr:::relative(rmd)
  )
})

test_that("wflow_status fails early if deleted subdirectory is current working directory", {

  if (.Platform$OS.type == "windows")
    skip("Current working directory cannot be deleted on Windows")

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  cwd <- fs::path_wd()
  on.exit(setwd(cwd), add = TRUE)

  subdir <- file.path(path, "sub")
  fs::dir_create(subdir)
  setwd(subdir)
  expect_silent(s <- wflow_status())
  fs::dir_delete(subdir)
  expect_error(wflow_status(), "The current working directory doesn't exist.")
})

test_that("wflow_status fails early if deleted root directory is current working directory", {

  if (.Platform$OS.type == "windows")
    skip("Current working directory cannot be deleted on Windows")

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  cwd <- fs::path_wd()
  on.exit(setwd(cwd), add = TRUE)

  setwd(path)
  expect_silent(s <- wflow_status())
  fs::dir_delete(path)
  expect_error(wflow_status(), "The current working directory doesn't exist.")
})

test_that("wflow_status fails early if deleted external directory is current working directory", {

  if (.Platform$OS.type == "windows")
    skip("Current working directory cannot be deleted on Windows")

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  cwd <- fs::path_wd()
  on.exit(setwd(cwd), add = TRUE)

  extdir <- fs::file_temp()
  on.exit(if (fs::dir_exists(extdir)) fs::dir_delete(extdir), add = TRUE)
  fs::dir_create(extdir)
  setwd(extdir)
  expect_silent(s <- wflow_status(project = path))
  fs::dir_delete(extdir)
  expect_error(wflow_status(project = path), "The current working directory doesn't exist.")
})

# Test wflow_paths -------------------------------------------------------------

# Most of this is redundant with wflow_status, so only testing different
# capabilities.

# wflow_status sets error_git = TRUE, so check that the default works.
test_that("wflow_paths returns NA if no Git repository and error_git = FALSE.", {
  git_original <- file.path(site_dir, ".git")
  git_replace <-  file.path(site_dir, ".git2")
  on.exit(file.rename(git_replace, git_original))
  file.rename(git_original, git_replace)
  expect_silent(p <- wflow_paths(project = site_dir))
  expect_identical(p$git, NA_character_)
})

test_that("wflow_paths is not confused by multiple similar _site.yml files in the same directory", {
  p1 <- wflow_paths(project = site_dir)
  spurious <- file.path(site_dir, "analysis", "_site.yml.bk")
  on.exit(fs::file_delete(spurious))
  fs::file_create(spurious)
  expect_silent(p2 <- wflow_paths(project = site_dir))
  expect_identical(p2, p1)
})

test_that("wflow_paths throws error if multiple _site.yml files in top-level directories", {
  extra <- file.path(site_dir, "code", "_site.yml")
  on.exit(fs::file_delete(extra))
  fs::file_create(extra)
  expect_error(wflow_paths(project = site_dir),
               "Found more than one _site.yml file.")
})

test_that("wflow_paths throws error if output_dir field not set in _site.yml", {
  site_yml <- file.path(site_dir, "analysis", "_site.yml")
  site_yml_tmp <- file.path(tempdir(), "_site.yml")
  on.exit(file.rename(site_yml_tmp, site_yml))
  file.rename(site_yml, site_yml_tmp)
  fs::file_create(site_yml)
  expect_error(wflow_paths(project = site_dir), "output_dir")
})

test_that("wflow_paths does *not* throw warning if docs/ directory is missing", {
  docs <- file.path(site_dir, "docs")
  docs_tmp <- fs::file_temp("docs-")
  on.exit(file.rename(docs_tmp, docs))
  file.rename(docs, docs_tmp)
  expect_silent(wflow_paths(project = site_dir))
})

# Test print.wflow_status ------------------------------------------------------

test_that("wflow_status includes Git status by default", {

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  status <- wflow_status(project = path)
  status_print <- utils::capture.output(print(status))
  expect_true("The current Git status is: working directory clean" %in% status_print)

  # modify a file in docs/. Status should not change
  fs::file_create(file.path(status$docs, "generated.txt"))

  status <- wflow_status(project = path)
  status_print <- utils::capture.output(print(status))
  expect_true("The current Git status is: working directory clean" %in% status_print)

  # Add a file in analysis/. Status should be untracked
  rmd <- file.path(status$analysis, "new.Rmd")
  fs::file_create(rmd)
  # Edit existing file. Status should be modified
  index <- file.path(status$analysis, "index.Rmd")
  cat("Edit\n", file = index, append = TRUE)

  status <- wflow_status(project = path)
  status_print <- utils::capture.output(print(status))
  expect_true("The current Git status is:" %in% status_print)
  expect_true(sum(
    stringr::str_detect(status_print,
                        glue::glue("\\s+untracked\\s+untracked\\s+{rmd}"))) == 1)
  expect_true(sum(
    stringr::str_detect(status_print,
                        glue::glue("\\s+unstaged\\s+modified\\s+{index}"))) == 1)
})

test_that("wflow_status can omit Git status", {

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  status <- wflow_status(include_git_status = FALSE, project = path)
  status_print <- utils::capture.output(print(status))
  expect_false("The current Git status is:" %in% status_print)

  # modify a file in docs/. Status should not change
  fs::file_create(file.path(status$docs, "generated.txt"))

  status <- wflow_status(include_git_status = FALSE, project = path)
  status_print <- utils::capture.output(print(status))
  expect_false("The current Git status is:" %in% status_print)

  # modify a file in analysis/. Status should change
  fs::file_create(file.path(status$analysis, "new.Rmd"))

  status <- wflow_status(include_git_status = FALSE, project = path)
  status_print <- utils::capture.output(print(status))
  expect_false("The current Git status is:" %in% status_print)
})
