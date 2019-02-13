context("wflow_open")

# Setup ------------------------------------------------------------------------

# start project in a tempdir
site_dir <- tempfile("test-wflow_open-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))
site_dir <- workflowr:::relative(site_dir)
on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))
p <- workflowr:::wflow_paths(project = site_dir)

# Load helper function local_no_gitconfig()
source("helpers.R", local = TRUE)

# Test wflow_open --------------------------------------------------------------

test_that("wflow_open creates a new file, but does not overwrite", {

  rmd <- wflow_open(file.path(p$analysis, "test.Rmd"), change_wd = FALSE,
                    edit_in_rstudio = FALSE, project = site_dir)
  expect_true(fs::file_exists(rmd$files))
  expect_output(print(rmd), "- New file\\(s\\):")
  expect_output(print(rmd), rmd$files)
  modification_time_pre <- file.mtime(rmd$files)
  Sys.sleep(2)
  rmd2 <- wflow_open(file.path(p$analysis, "test.Rmd"), change_wd = FALSE,
                     edit_in_rstudio = FALSE, project = site_dir)
  expect_identical(rmd2$files, rmd$files)
  expect_output(print(rmd2), "- Existing file\\(s\\):")
  expect_output(print(rmd2), rmd2$files)
  modification_time_post <- file.mtime(rmd2$files)
  expect_identical(modification_time_post, modification_time_pre)
})

test_that("wflow_open changes the working directory to the knit directory", {

  # Too many filepath edge cases on CRAN servers that are not caught on
  # windbuilder or AppVeyor
  skip_on_cran()

  cwd <- getwd()
  on.exit(setwd(cwd))
  wflow_yml <- absolute(file.path(p$root, "_workflowr.yml"))
  wflow_copy <- tempfile()
  fs::file_copy(wflow_yml, wflow_copy)
  on.exit(file.rename(wflow_copy, wflow_yml), add = TRUE)

  # Do not change working directory
  wd_pre <- getwd()
  expect_silent(rmd <- wflow_open(file.path(p$analysis, "do-not-change-wd.Rmd"),
                                  change_wd = FALSE,
                                  edit_in_rstudio = FALSE, project = site_dir))
  expect_true(fs::file_exists(rmd$files))
  wd_same <- getwd()
  expect_identical(wd_pre, wd_same)
  expect_output(print(rmd), "- Same working directory")
  expect_output(print(rmd), wd_same)

  # knit directory == root (default)
  rmd <- wflow_open(file.path(p$analysis, "do-change-wd.Rmd"),
                    change_wd = TRUE,
                    edit_in_rstudio = FALSE, project = site_dir)
  expect_true(fs::file_exists(rmd$files))
  p <- workflowr:::wflow_paths()
  expect_identical(getwd(), absolute(p$root))
  expect_output(print(rmd), "- New working directory")
  expect_output(print(rmd), getwd())
  expect_silent(rmd <- wflow_open(file.path(p$analysis, "no-need-to-change-wd.Rmd"),
                                  change_wd = TRUE, edit_in_rstudio = FALSE,
                                  project = "."))
  expect_true(fs::file_exists(rmd$files))
  expect_identical(getwd(), absolute(p$root))
  expect_output(print(rmd), "- Same working directory")
  expect_output(print(rmd), getwd())

  # knit directory == analysis
  yml <- yaml::read_yaml(wflow_yml)
  yml$knit_root_dir <- "analysis"
  yaml::write_yaml(yml, file = wflow_yml)
  rmd <- wflow_open(file.path(p$analysis, "change-to-analysis.Rmd"),
                    change_wd = TRUE,
                    edit_in_rstudio = FALSE, project = ".")
  expect_true(fs::file_exists(rmd$files))
  p <- workflowr:::wflow_paths()
  expect_identical(getwd(), absolute(p$analysis))
  expect_output(print(rmd), "- New working directory")
  expect_output(print(rmd), getwd())

  # knit directory == docs
  yml <- yaml::read_yaml(wflow_yml)
  yml$knit_root_dir <- "docs"
  yaml::write_yaml(yml, file = wflow_yml)
  rmd <- wflow_open(file.path(p$analysis, "change-to-docs.Rmd"),
                    change_wd = TRUE,
                    edit_in_rstudio = FALSE, project = ".")
  expect_true(fs::file_exists(rmd$files))
  p <- workflowr:::wflow_paths()
  expect_identical(getwd(), absolute(p$docs))
  expect_output(print(rmd), "- New working directory")
  expect_output(print(rmd), getwd())

  # knit directory == NULL (unset defaults to analysis/)
  yml <- yaml::read_yaml(wflow_yml)
  yml$knit_root_dir <- NULL
  yaml::write_yaml(yml, file = wflow_yml)
  rmd <- wflow_open(file.path(p$analysis, "unset.Rmd"),
                    change_wd = TRUE,
                    edit_in_rstudio = FALSE, project = ".")
  expect_true(fs::file_exists(rmd$files))
  p <- workflowr:::wflow_paths()
  expect_identical(getwd(), absolute(p$analysis))
  expect_output(print(rmd), "- New working directory")
  expect_output(print(rmd), getwd())

})

test_that("wflow_open can accept multiple files", {

  rmd_multi <- file.path(p$analysis, paste0(1:3, ".Rmd"))
  rmd <- wflow_open(rmd_multi, change_wd = FALSE, edit_in_rstudio = FALSE,
                    project = site_dir)
  expect_true(all(fs::file_exists(rmd$files)))
  modification_time_pre <- file.mtime(rmd$files)
  Sys.sleep(2)
  rmd2 <- wflow_open(rmd_multi, change_wd = FALSE, edit_in_rstudio = FALSE,
                     project = site_dir)
  expect_identical(rmd2$files, rmd$files)
  modification_time_post <- file.mtime(rmd$files)
  expect_identical(modification_time_post, modification_time_pre)
})

# test_that("wflow_open can accept basename, full paths, and wrong paths", {
#
#   rmd_paths <- c("basename.Rmd",
#                  file.path(site_dir, "analysis", "full.Rmd"),
#                  file.path(site_dir, "code", "wrong.Rmd"))
#   rmd <- wflow_open(rmd_paths, change_wd = FALSE, edit_in_rstudio = FALSE,
#                     project = site_dir)
#   expect_true(all(fs::file_exists(rmd)))
#   modification_time_pre <- file.mtime(rmd)
#   Sys.sleep(2)
#   rmd2 <- wflow_open(rmd_paths, change_wd = FALSE, edit_in_rstudio = FALSE,
#                      project = site_dir)
#   expect_identical(rmd2, rmd)
#   modification_time_post <- file.mtime(rmd)
#   expect_identical(modification_time_post, modification_time_pre)
# })

test_that("wflow_open can save outside of analysis/ when project = NULL", {

  # When project = NULL, wflow_open will create output directories if needed,
  # but will not change the working directory.
  cwd <- getwd()
  on.exit(setwd(cwd))
  location_exist <- tempfile("test-wflow_open-exist-")
  fs::dir_create(location_exist)
  location_exist <- workflowr:::absolute(location_exist)
  on.exit(unlink(location_exist, recursive = TRUE, force = TRUE), add = TRUE)
  testfile1 <- file.path(location_exist, "exist.Rmd")
  location_nonexist <- tempfile("test-wflow_open-nonexist-")
  on.exit(unlink(location_nonexist, recursive = TRUE, force = TRUE), add = TRUE)
  testfile2 <- file.path(location_nonexist, "nonexist.Rmd")

  o <- wflow_open(c(testfile1, testfile2), change_wd = TRUE,
                  edit_in_rstudio = FALSE, project = NULL)

  expect_true(all(fs::file_exists(o$files)))
  # Fix the symlink now that the file has been created
  testfile2 <- workflowr:::absolute(testfile2)
  expect_identical(o$files, c(testfile1, testfile2))
  # Confirm the working directory was **not** changed
  expect_identical(getwd(), cwd)
})

test_that("wflow_open can create a file when no Git repo or config present", {

  skip_on_cran()

  # local_no_gitconfig() is defined in tests/testthat/helpers.R
  local_no_gitconfig("-workflowr")

  # First in the context of a workflowr project
  x <- wflow_start(tempfile(), git = FALSE, change_wd = FALSE)
  p <- wflow_paths(project = x$directory)
  rmd <- file.path(p$analysis, "new.Rmd")
  o <- wflow_open(files = rmd, change_wd = FALSE,
                  edit_in_rstudio = FALSE, project = p$root)
  expect_true(fs::file_exists(rmd))

  # Second outside the context of a workflowr project
  cwd <- getwd()
  x <-tempfile()
  fs::dir_create(x)
  x <- workflowr:::absolute(x)
  rmd <- file.path(x, "new.Rmd")
  o <- wflow_open(files = rmd, edit_in_rstudio = FALSE, project = NULL)
  expect_identical(getwd(), cwd)
  expect_true(fs::file_exists(rmd))
})

test_that("wflow_open sends warning if used in workflowrBeta project", {
  tmp_dir <- tempfile()
  fs::dir_create(tmp_dir)
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  file.copy("files/test-wflow_update/pre/.", tmp_dir, recursive = TRUE)
  fs::dir_create(file.path(tmp_dir, "docs"))
  git2r::init(tmp_dir)
  r <- git2r::repository(tmp_dir)
  git2r::config(r, user.name = "Test Name", user.email = "test@email")
  workflowr:::git2r_add(r, ".")
  git2r::commit(r, "Initial commit.")

  rmd <- file.path(tmp_dir, "analysis", "new.Rmd")
  expect_warning(wflow_open(rmd, change_wd = FALSE, edit_in_rstudio = FALSE,
                            project = tmp_dir),
                 "It appears that your site was created")
  expect_true(fs::file_exists(rmd))
})

# Errors -----------------------------------------------------------------------

test_that("wflow_open rejects filenames without Rmd or rmd extension", {

  expect_error(wflow_open(file.path(p$analysis, "invalid-ext.md"), change_wd = FALSE, edit_in_rstudio = FALSE,
                          project = site_dir),
               "R Markdown files must have the extension Rmd or rmd.")
  expect_error(wflow_open("no-ext", change_wd = FALSE, edit_in_rstudio = FALSE,
                          project = site_dir),
               "R Markdown files must have the extension Rmd or rmd.")
  expect_silent(wflow_open(file.path(p$analysis, "valid-ext.Rmd"), change_wd = FALSE, edit_in_rstudio = FALSE,
                           project = site_dir))
  expect_silent(wflow_open(file.path(p$analysis, "valid-ext.rmd"), change_wd = FALSE, edit_in_rstudio = FALSE,
                           project = site_dir))
})

test_that("wflow_open throws error if not in workflowr project and project!=NULL", {
  x <- tempfile()
  fs::dir_create(x)
  on.exit(unlink(x, recursive = TRUE))
  rmd <- file.path(x, "test.Rmd")
  expect_error(wflow_open(rmd, change_wd = FALSE, edit_in_rstudio = FALSE),
               "This isn't a workflowr project")
  expect_silent(wflow_open(rmd, change_wd = FALSE, edit_in_rstudio = FALSE,
                           project = NULL))
  expect_true(fs::file_exists(rmd))
})

test_that("wflow_open throws error if in workflowr project, but Rmd files outside workflowr project", {
  rmd1 <- absolute(tempfile(fileext = ".Rmd"))
  rmd2 <- absolute(file.path(p$analysis, "index.Rmd"))
  rmd3 <- absolute(tempfile(fileext = ".Rmd"))
  on.exit(unlink(c(rmd1, rmd3)))

  expect_error(wflow_open(c(rmd1, rmd2, rmd3),
                          change_wd = FALSE, edit_in_rstudio = FALSE,
                          project = site_dir),
               "Argument \"files\" specifies at least")
  expect_error(wflow_open(c(rmd1, rmd2, rmd3),
                          change_wd = FALSE, edit_in_rstudio = FALSE,
                          project = site_dir),
               rmd1)
  expect_error(wflow_open(c(rmd1, rmd2, rmd3),
                          change_wd = FALSE, edit_in_rstudio = FALSE,
                          project = site_dir),
               rmd3)
  expect_silent(o <- wflow_open(c(rmd1, rmd2, rmd3),
                                change_wd = FALSE, edit_in_rstudio = FALSE,
                                project = NULL))
  expect_true(all(fs::file_exists(c(rmd1, rmd2, rmd3))))
})

test_that("wflow_open throws error if file is not in R Markdown directory", {
  rmd1 <- absolute(file.path(p$docs, "docs.Rmd"))
  rmd2 <- absolute(file.path(p$analysis, "index.Rmd"))
  rmd3 <- absolute(file.path(p$root, "root.Rmd"))
  dir_mistake <- absolute(file.path(p$analysis, "analysis"))
  rmd4 <- absolute(file.path(dir_mistake, "mistake.Rmd"))
  on.exit(unlink(c(rmd1, rmd3, dir_mistake), recursive = TRUE))

  expect_error(wflow_open(c(rmd1, rmd2, rmd3, rmd4),
                            change_wd = FALSE, edit_in_rstudio = FALSE,
                            project = site_dir),
                 "Argument \"files\" specifies at least")
  expect_false(all(fs::file_exists(c(rmd1, rmd3, rmd4))))
  expect_error(wflow_open(c(rmd1, rmd2, rmd3, rmd4),
                            change_wd = FALSE, edit_in_rstudio = FALSE,
                            project = site_dir),
                 rmd1)
  expect_error(wflow_open(c(rmd1, rmd2, rmd3, rmd4),
                            change_wd = FALSE, edit_in_rstudio = FALSE,
                            project = site_dir),
                 rmd3)
  expect_error(wflow_open(c(rmd1, rmd2, rmd3, rmd4),
                            change_wd = FALSE, edit_in_rstudio = FALSE,
                            project = site_dir),
                 rmd4)

  # No warning if project=NULL
  expect_silent(o <- wflow_open(c(rmd1, rmd2, rmd3, rmd4),
                                change_wd = FALSE, edit_in_rstudio = FALSE,
                                project = NULL))
  expect_true(all(fs::file_exists(c(rmd1, rmd2, rmd3, rmd4))))
})
