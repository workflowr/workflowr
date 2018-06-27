context("wflow_open")

# Setup ------------------------------------------------------------------------

# start project in a tempdir
site_dir <- tempfile("test-wflow_open-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE,
                             user.name = "Test Name", user.email = "test@email"))
site_dir <- workflowr:::relative(site_dir)
on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))
p <- workflowr:::wflow_paths(project = site_dir)

# Test wflow_open --------------------------------------------------------------

test_that("wflow_open creates a new file, but does not overwrite", {

  rmd <- wflow_open(file.path(p$analysis, "test.Rmd"), change_wd = FALSE,
                    open_file = FALSE, project = site_dir)
  expect_true(file.exists(rmd$files))
  expect_output(print(rmd), "- New file\\(s\\):")
  expect_output(print(rmd), rmd$files)
  modification_time_pre <- file.mtime(rmd$files)
  Sys.sleep(2)
  rmd2 <- wflow_open(file.path(p$analysis, "test.Rmd"), change_wd = FALSE,
                     open_file = FALSE, project = site_dir)
  expect_identical(rmd2$files, rmd$files)
  expect_output(print(rmd2), "- Existing file\\(s\\):")
  expect_output(print(rmd2), rmd2$files)
  modification_time_post <- file.mtime(rmd2$files)
  expect_identical(modification_time_post, modification_time_pre)
})

test_that("wflow_open changes the working directory to the knit directory", {

  cwd <- getwd()
  on.exit(setwd(cwd))
  wflow_yml <- absolute(file.path(p$root, "_workflowr.yml"))
  wflow_copy <- tempfile()
  file.copy(wflow_yml, wflow_copy)
  on.exit(file.rename(wflow_copy, wflow_yml), add = TRUE)

  # Do not change working directory
  wd_pre <- getwd()
  expect_silent(rmd <- wflow_open(file.path(p$analysis, "do-not-change-wd.Rmd"),
                                  change_wd = FALSE,
                                  open_file = FALSE, project = site_dir))
  expect_true(file.exists(rmd$files))
  wd_same <- getwd()
  expect_identical(wd_pre, wd_same)
  expect_output(print(rmd), "- Same working directory")
  expect_output(print(rmd), wd_same)

  # knit directory == root (default)
  rmd <- wflow_open(file.path(p$analysis, "do-change-wd.Rmd"),
                    change_wd = TRUE,
                    open_file = FALSE, project = site_dir)
  expect_true(file.exists(rmd$files))
  p <- workflowr:::wflow_paths()
  expect_identical(getwd(), absolute(p$root))
  expect_output(print(rmd), "- New working directory")
  expect_output(print(rmd), getwd())
  expect_silent(rmd <- wflow_open(file.path(p$analysis, "no-need-to-change-wd.Rmd"),
                                  change_wd = TRUE, open_file = FALSE,
                                  project = "."))
  expect_true(file.exists(rmd$files))
  expect_identical(getwd(), absolute(p$root))
  expect_output(print(rmd), "- Same working directory")
  expect_output(print(rmd), getwd())

  # knit directory == analysis
  yml <- yaml::read_yaml(wflow_yml)
  yml$knit_root_dir <- "analysis"
  yaml::write_yaml(yml, file = wflow_yml)
  rmd <- wflow_open(file.path(p$analysis, "change-to-analysis.Rmd"),
                    change_wd = TRUE,
                    open_file = FALSE, project = ".")
  expect_true(file.exists(rmd$files))
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
                    open_file = FALSE, project = ".")
  expect_true(file.exists(rmd$files))
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
                    open_file = FALSE, project = ".")
  expect_true(file.exists(rmd$files))
  p <- workflowr:::wflow_paths()
  expect_identical(getwd(), absolute(p$analysis))
  expect_output(print(rmd), "- New working directory")
  expect_output(print(rmd), getwd())

})

test_that("wflow_open can accept multiple files", {

  rmd_multi <- file.path(p$analysis, paste0(1:3, ".Rmd"))
  rmd <- wflow_open(rmd_multi, change_wd = FALSE, open_file = FALSE,
                    project = site_dir)
  expect_true(all(file.exists(rmd$files)))
  modification_time_pre <- file.mtime(rmd$files)
  Sys.sleep(2)
  rmd2 <- wflow_open(rmd_multi, change_wd = FALSE, open_file = FALSE,
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
#   rmd <- wflow_open(rmd_paths, change_wd = FALSE, open_file = FALSE,
#                     project = site_dir)
#   expect_true(all(file.exists(rmd)))
#   modification_time_pre <- file.mtime(rmd)
#   Sys.sleep(2)
#   rmd2 <- wflow_open(rmd_paths, change_wd = FALSE, open_file = FALSE,
#                      project = site_dir)
#   expect_identical(rmd2, rmd)
#   modification_time_post <- file.mtime(rmd)
#   expect_identical(modification_time_post, modification_time_pre)
# })

test_that("wflow_open can save outside of analysis/ when project = NULL", {

  # When project = NULL, wflow_open will create output directories if needed, and
  # switches the working directory to the path of the first input file.
  cwd <- getwd()
  on.exit(setwd(cwd))
  location_exist <- tempfile("test-wflow_open-exist-")
  dir.create(location_exist)
  location_exist <- workflowr:::absolute(location_exist)
  on.exit(unlink(location_exist, recursive = TRUE, force = TRUE), add = TRUE)
  testfile1 <- file.path(location_exist, "exist.Rmd")
  location_nonexist <- tempfile("test-wflow_open-nonexist-")
  on.exit(unlink(location_nonexist, recursive = TRUE, force = TRUE), add = TRUE)
  testfile2 <- file.path(location_nonexist, "nonexist.Rmd")

  o <- wflow_open(c(testfile1, testfile2), change_wd = TRUE,
                  open_file = FALSE, project = NULL)

  expect_true(all(file.exists(o$files)))
  # Fix the symlink now that the file has been created
  testfile2 <- workflowr:::absolute(testfile2)
  expect_identical(o$files, c(testfile1, testfile2))
})

# Errors -----------------------------------------------------------------------

test_that("wflow_open rejects filenames without Rmd or rmd extension", {

  expect_error(wflow_open(file.path(p$analysis, "invalid-ext.md"), change_wd = FALSE, open_file = FALSE,
                          project = site_dir),
               "R Markdown files must have the extension Rmd or rmd.")
  expect_error(wflow_open("no-ext", change_wd = FALSE, open_file = FALSE,
                          project = site_dir),
               "R Markdown files must have the extension Rmd or rmd.")
  expect_silent(wflow_open(file.path(p$analysis, "valid-ext.Rmd"), change_wd = FALSE, open_file = FALSE,
                           project = site_dir))
  expect_silent(wflow_open(file.path(p$analysis, "valid-ext.rmd"), change_wd = FALSE, open_file = FALSE,
                           project = site_dir))
})

test_that("wflow_open throws error if not in workflowr project and project!=NULL", {
  x <- tempfile()
  dir.create(x)
  on.exit(unlink(x, recursive = TRUE))
  rmd <- file.path(x, "test.Rmd")
  expect_error(wflow_open(rmd, change_wd = FALSE, open_file = FALSE),
               "This isn't a workflowr project")
  expect_silent(wflow_open(rmd, change_wd = FALSE, open_file = FALSE,
                           project = NULL))
  expect_true(file.exists(rmd))
})

test_that("wflow_open throws error if in workflowr project, but Rmd files outside workflowr project", {
  rmd1 <- absolute(tempfile(fileext = ".Rmd"))
  rmd2 <- absolute(file.path(p$analysis, "index.Rmd"))
  rmd3 <- absolute(tempfile(fileext = ".Rmd"))
  on.exit(unlink(c(rmd1, rmd3)))

  expect_error(wflow_open(c(rmd1, rmd2, rmd3),
                          change_wd = FALSE, open_file = FALSE,
                          project = site_dir),
               "The following file\\(s\\) are not within the workflowr project")
  expect_error(wflow_open(c(rmd1, rmd2, rmd3),
                          change_wd = FALSE, open_file = FALSE,
                          project = site_dir),
               rmd1)
  expect_error(wflow_open(c(rmd1, rmd2, rmd3),
                          change_wd = FALSE, open_file = FALSE,
                          project = site_dir),
               rmd3)
  expect_silent(o <- wflow_open(c(rmd1, rmd2, rmd3),
                                change_wd = FALSE, open_file = FALSE,
                                project = NULL))
  expect_true(all(file.exists(c(rmd1, rmd2, rmd3))))
})

test_that("wflow_open sends warning if file is not in R Markdown directory", {
  rmd1 <- absolute(file.path(p$docs, "docs.Rmd"))
  rmd2 <- absolute(file.path(p$analysis, "index.Rmd"))
  rmd3 <- absolute(file.path(p$root, "root.Rmd"))
  on.exit(unlink(c(rmd1, rmd3)))

  expect_warning(wflow_open(c(rmd1, rmd2, rmd3),
                            change_wd = FALSE, open_file = FALSE,
                            project = site_dir),
                 "not within the R Markdown directory")
  expect_true(all(file.exists(c(rmd1, rmd2, rmd3))))
  expect_warning(wflow_open(c(rmd1, rmd2, rmd3),
                            change_wd = FALSE, open_file = FALSE,
                            project = site_dir),
                 rmd1)
  expect_warning(wflow_open(c(rmd1, rmd2, rmd3),
                            change_wd = FALSE, open_file = FALSE,
                            project = site_dir),
                 rmd3)
  # No warning if project=NULL
  expect_silent(o <- wflow_open(c(rmd1, rmd2, rmd3),
                                change_wd = FALSE, open_file = FALSE,
                                project = NULL))
})
