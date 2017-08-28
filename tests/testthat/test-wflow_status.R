context("wflow_status")

# Setup ------------------------------------------------------------------------

# Setup workflowr project for testing
site_dir <- workflowr:::tempfile("test-wflow_status-",
                                 tmpdir = workflowr:::normalizePath("/tmp"))
suppressMessages(wflow_start(site_dir, change_wd = FALSE))
# Cleanup
on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))

# Test wflow_status ------------------------------------------------------------

s <- wflow_status(project = site_dir)

wd <- getwd()

test_that("wflow_status identifies root directory.", {
  expected <- workflowr:::relpath(site_dir, start = wd)
  actual <- s$root
  expect_identical(actual, expected)
})

test_that("wflow_status identifies analysis directory.", {
  expected <- file.path(site_dir, "analysis")
  expected <- workflowr:::relpath(expected, start = wd)
  actual <- s$analysis
  expect_identical(actual, expected)
})

test_that("wflow_status identifies docs directory.", {
  expected <- file.path(site_dir, "docs")
  expected <- workflowr:::relpath(expected, start = wd)
  actual <- s$docs
  expect_identical(actual, expected)
})

test_that("wflow_status identifies Git directory.", {
  expected <- file.path(site_dir, ".git")
  expected <- workflowr:::relpath(expected, start = wd)
  actual <- s$git
  expect_identical(actual, expected)
})

# Create a new untracked file that will have status Scr for Scratch
rmd_scr <- file.path(s$analysis, "scratch.Rmd")
file.create(rmd_scr)
# Publish index.Rmd
rmd_pub <- file.path(s$analysis, "index.Rmd")
suppressMessages(wflow_publish(rmd_pub, "Publish the index",
                               project = site_dir))
# Publish and then modify about.Rmd to have status Mod for Modified
rmd_mod <- file.path(s$analysis, "about.Rmd")
suppressMessages(wflow_publish(rmd_mod, "Publish the about page",
                               project = site_dir))
cat("edit", file = rmd_mod, append = TRUE)
# license.Rmd still has status Unp for Unpublished
rmd_unp <- file.path(s$analysis, "license.Rmd")

test_that("wflow_status classifies files when run from outside workflowr project.", {
  s_tmp <- wflow_status(project = site_dir)
  expect_true(s_tmp$status[rmd_scr, "scratch"])
  expect_true(s_tmp$status[rmd_pub, "published"])
  expect_true(s_tmp$status[rmd_mod, "modified"])
  expect_true(s_tmp$status[rmd_unp, "unpublished"])
})

test_that("wflow_status classifies files when run from root of workflowr project.", {
  rmd_scr <- workflowr:::relpath_vec(rmd_scr, start = s$root)
  rmd_pub <- workflowr:::relpath_vec(rmd_pub, start = s$root)
  rmd_mod <- workflowr:::relpath_vec(rmd_mod, start = s$root)
  rmd_unp <- workflowr:::relpath_vec(rmd_unp, start = s$root)
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(s$root)
  s_tmp <- wflow_status()
  expect_true(s_tmp$status[rmd_scr, "scratch"])
  expect_true(s_tmp$status[rmd_pub, "published"])
  expect_true(s_tmp$status[rmd_mod, "modified"])
  expect_true(s_tmp$status[rmd_unp, "unpublished"])
})

test_that("wflow_status classifies files when run from analysis/.", {
  rmd_scr <- workflowr:::relpath_vec(rmd_scr, start = s$analysis)
  rmd_pub <- workflowr:::relpath_vec(rmd_pub, start = s$analysis)
  rmd_mod <- workflowr:::relpath_vec(rmd_mod, start = s$analysis)
  rmd_unp <- workflowr:::relpath_vec(rmd_unp, start = s$analysis)
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(s$analysis)
  s_tmp <- wflow_status()
  expect_true(s_tmp$status[rmd_scr, "scratch"])
  expect_true(s_tmp$status[rmd_pub, "published"])
  expect_true(s_tmp$status[rmd_mod, "modified"])
  expect_true(s_tmp$status[rmd_unp, "unpublished"])
})

test_that("wflow_status classifies files when run from docs/.", {
  rmd_scr <- workflowr:::relpath_vec(rmd_scr, start = s$docs)
  rmd_pub <- workflowr:::relpath_vec(rmd_pub, start = s$docs)
  rmd_mod <- workflowr:::relpath_vec(rmd_mod, start = s$docs)
  rmd_unp <- workflowr:::relpath_vec(rmd_unp, start = s$docs)
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(s$docs)
  s_tmp <- wflow_status()
  expect_true(s_tmp$status[rmd_scr, "scratch"])
  expect_true(s_tmp$status[rmd_pub, "published"])
  expect_true(s_tmp$status[rmd_mod, "modified"])
  expect_true(s_tmp$status[rmd_unp, "unpublished"])
})

test_that("wflow_status reports only specified files", {
  s_tmp <- wflow_status(rmd_scr, project = site_dir)
  expect_identical(rownames(s_tmp$status), rmd_scr)
  s_tmp <- wflow_status(c(rmd_scr, rmd_unp, rmd_pub), project = site_dir)
  expect_identical(rownames(s_tmp$status), c(rmd_scr, rmd_unp, rmd_pub))
})

# Warnings and Errors ----------------------------------------------------------

test_that("wflow_status throws error if not in workflowr project.", {
  non_project <- workflowr:::tempfile("non-project-",
                                      tmpdir = workflowr:::normalizePath("/tmp"))
  dir.create(non_project, recursive = TRUE)
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

test_that("wflow_status throws error if no Git repository.", {
  git_original <- file.path(site_dir, ".git")
  git_replace <-  file.path(site_dir, ".git2")
  on.exit(file.rename(git_replace, git_original))
  file.rename(git_original, git_replace)
  expect_error(s <- wflow_status(project = site_dir),
               "A Git repository is required for this functionality.")
})
