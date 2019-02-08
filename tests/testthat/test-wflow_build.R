context("wflow_build")

# Setup ------------------------------------------------------------------------

# start project in a tempdir
site_dir <- tempfile("test-wflow_build-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE, user.name = "Test Name",
                             user.email = "test@email"))
on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))
site_dir <- workflowr:::absolute(site_dir)
s <- wflow_status(project = site_dir)

rmd <- rownames(s$status)
stopifnot(length(rmd) > 0)
# Expected html files
html <- workflowr:::to_html(rmd, outdir = s$docs)

# Test wflow_build -------------------------------------------------------------

test_that("wflow_build builds the specified files", {

  skip_on_cran()

  # Dry run for file 1
  expect_silent(actual <- wflow_build(rmd[1], dry_run = TRUE,
                                      project = site_dir))
  expect_identical(actual$built, rmd[1])
  expect_false(fs::file_exists(html[1]))
  # Build file 1
  expect_message(actual <- wflow_build(rmd[1], view = FALSE, dry_run = FALSE,
                                       project = site_dir),
                 rmd[1])
  expect_identical(actual$built, rmd[1])
  expect_true(fs::file_exists(html[1]))
  # Dry run for files 2 & 3
  expect_silent(actual <- wflow_build(rmd[2:3], dry_run = TRUE,
                                      project = site_dir))
  expect_identical(actual$built, rmd[2:3])
  expect_false(any(fs::file_exists(html[2:3])))
  # Build files 2 & 3
  expect_message(actual <- wflow_build(rmd[2:3], view = FALSE, dry_run = FALSE,
                                       project = site_dir),
                 rmd[2])
  expect_identical(actual$built, rmd[2:3])
  expect_true(all(fs::file_exists(html[2:3])))
})

test_that("wflow_build can run in 'make' mode", {

  skip_on_cran()

  # Reset modifications of rmd files. It is important to wait a couple
  # seconds so that the modification times are different.
  Sys.sleep(2)
  system2("touch", args = rmd)
  expect_silent(actual <- wflow_build(dry_run = TRUE, project = site_dir))
  expect_identical(actual$built, rmd)
  expect_true(actual$make)
  expect_message(actual <- wflow_build(view = FALSE, project = site_dir),
                 rmd[1])
  expect_identical(actual$built, rmd)

  # No file should be built now.
  expect_silent(actual <- wflow_build(view = FALSE, project = site_dir))
  expect_identical(actual$built, character(0))

  # Reset modification of file 1 only. It is important to wait a couple
  # seconds so that the modification times are different.
  Sys.sleep(2)
  system2("touch", args = rmd[1])
  expect_message(actual <- wflow_build(view = FALSE, project = site_dir),
                 rmd[1])
  expect_identical(actual$built, rmd[1])
})

# Fixed error in which 'make' didn't work with relative paths from the root
# directory. This set of tests ensures that this won't happen again.
test_that("wflow_build can run in 'make' mode from within project", {

  skip_on_cran()

  cwd <- getwd()
  setwd(site_dir)
  on.exit(setwd(cwd))
  rmd_local <- Sys.glob("analysis/*Rmd")
  html_local <- workflowr:::to_html(rmd_local, outdir = "docs")

  # Reset modifications of rmd files. It is important to wait a couple
  # seconds so that the modification times are different.
  Sys.sleep(2)
  system2("touch", args = rmd_local)
  expect_silent(actual <- wflow_build(dry_run = TRUE))
  expect_identical(actual$built, rmd_local)
  expect_true(actual$make)
  expect_message(actual <- wflow_build(view = FALSE), rmd_local[1])
  expect_identical(actual$built, rmd_local)

  # No file should be built now.
  expect_silent(actual <- wflow_build())
  expect_identical(actual$built, character(0))

  # Reset modification of file 1 only. It is important to wait a couple
  # seconds so that the modification times are different.
  Sys.sleep(2)
  system2("touch", args = rmd_local[1])
  expect_message(actual <- wflow_build(view = FALSE), rmd_local[1])
  expect_identical(actual$built, rmd_local[1])
})

test_that("wflow_build update builds published files with modifications", {

  skip_on_cran()

  # Publish the files
  suppressMessages(wflow_publish(files = rmd, view = FALSE, project = site_dir))

  cat("edit", file = rmd[1], append = TRUE)
  wflow_git_commit(rmd[1], project = site_dir)
  expect_silent(actual <- wflow_build(update = TRUE, dry_run = TRUE,
                                      project = site_dir))
  expect_identical(actual$built, rmd[1])
  expect_true(actual$update)
  expect_message(actual <- wflow_build(update = TRUE, view = FALSE,
                                       project = site_dir),
                 rmd[1])
  expect_identical(actual$built, rmd[1])
})

test_that("wflow_build republish builds all published files", {

  skip_on_cran()

  wflow_build(view = FALSE, project = site_dir)
  html_mtime_pre <- file.mtime(html)
  Sys.sleep(2)
  expect_message(actual <- wflow_build(view = FALSE, republish = TRUE,
                                       project = site_dir),
                 rmd[1])
  expect_true(actual$republish)
  expect_identical(actual$built, rmd)
  html_mtime_post <- file.mtime(html)
  expect_true(all(html_mtime_post > html_mtime_pre))
})

# The default is to build a file in its own separate R session to avoid
# conflicts in the variable names and loaded packages between files. However, it
# may be useful for debugging to build the file directly in the R console. To
# test the difference, the file `local.Rmd` has an undefined variable, and it
# should only be able to access it from the global environment when built
# locally.
test_that("Only locally built files can access variables in the global environment", {

  skip_on_cran()

  fs::file_copy("files/test-wflow_build/global-variable.Rmd", s$analysis)
  rmd_local <- file.path(s$analysis, "global-variable.Rmd")
  html_local <- workflowr:::to_html(rmd_local, outdir = s$docs)
  on.exit(fs::file_delete(c(rmd_local, html_local)))
  # Create a variable in the global environment
  # https://stackoverflow.com/a/25096276/2483477
  env <- globalenv()
  env$global_variable <- 1
  stopifnot(exists("global_variable", envir = env))
  expect_error(utils::capture.output(wflow_build(rmd_local, view = FALSE,
                                                 project = site_dir)),
               "object 'global_variable' not found")
  expect_false(fs::file_exists(html_local))
  utils::capture.output(wflow_build(rmd_local, local = TRUE, view = FALSE,
                                    project = site_dir))
  expect_true(fs::file_exists(html_local))
  # Remove the global variable
  rm("global_variable", envir = env)
  stopifnot(!exists("global_variable", envir = env))
})

# The test file local.Rmd loads the package "tools" and defines the variable
# `local_variable`.
test_that("Only locally built files add packages/variables to global environment", {

  skip_on_cran()

  fs::file_copy("files/test-wflow_build/local.Rmd", s$analysis)
  rmd_local <- file.path(s$analysis, "local.Rmd")
  html_local <- workflowr:::to_html(rmd_local, outdir = s$docs)
  on.exit(fs::file_delete(c(rmd_local, html_local)))
  on.exit(detach("package:tools"), add = TRUE)
  # Build file externally
  utils::capture.output(wflow_build(rmd_local, view = FALSE,
                                    project = site_dir))
  expect_false("package:tools" %in% search())
  expect_false(exists("local_variable", envir = .GlobalEnv))
  # Build file locally
  utils::capture.output(wflow_build(rmd_local, local = TRUE, view = FALSE,
                                    project = site_dir))
  expect_true("package:tools" %in% search())
  expect_true(exists("local_variable", envir = .GlobalEnv))
  # Remove `local_variable`
  rm("local_variable", envir = .GlobalEnv)
  stopifnot(!exists("global_variable", envir = .GlobalEnv))
})

test_that("wflow_build only builds files starting with _ when specified", {

  skip_on_cran()

  rmd_ignore <- file.path(s$analysis, "_ignore.Rmd")
  fs::file_copy("files/example.Rmd", rmd_ignore)
  html_ignore <- workflowr:::to_html(rmd_ignore, outdir = s$docs)
  # Ignored by default "make"-mode
  expect_silent(actual <- wflow_build(view = FALSE, project = site_dir))
  expect_false(fs::file_exists(html_ignore))
  expect_equal(length(actual$built), 0)
  # Built when directly specified
  expect_message(actual <- wflow_build(rmd_ignore, view = FALSE,
                                       project = site_dir),
                 rmd_ignore)
  expect_true(fs::file_exists(html_ignore))
  expect_identical(actual$built, rmd_ignore)
})

test_that("wflow_build uses tempdir() to save log files by default", {

  skip_on_cran()

  expected <- workflowr:::absolute(file.path(tempdir(), "workflowr"))
  actual <- wflow_build(rmd[1], view = FALSE, project = site_dir)
  expect_identical(expected, actual$log_dir)
})

test_that("wflow_build accepts custom directory to save log files", {

  skip_on_cran()

  expected <- workflowr:::absolute(file.path(site_dir, "log"))
  actual <- wflow_build(rmd[1], view = FALSE, log_dir = expected,
                        project = site_dir)
  expect_true(fs::dir_exists(expected))
  expect_identical(expected, actual$log_dir)
})

test_that("wflow_build removes unused figure files if clean_fig_files = TRUE", {

  skip_on_cran()

  # Build a file that has 2 plots from 2 unnamed chunks
  file_w_figs <- file.path(s$analysis, "fig.Rmd")
  fs::file_copy("files/test-wflow_build/figure-v01.Rmd", file_w_figs)
  build_v01 <- wflow_build(file_w_figs, view = FALSE, clean_fig_files = TRUE,
                           project = site_dir)
  figs_analysis_v01 <- file.path(s$analysis, "figure", basename(file_w_figs),
                                 c("unnamed-chunk-1-1.png", "unnamed-chunk-2-1.png"))
  expect_false(all(fs::file_exists(figs_analysis_v01))) # moved by wflow_site()
  figs_docs_v01 <- file.path(s$docs, "figure", basename(file_w_figs),
                             c("unnamed-chunk-1-1.png", "unnamed-chunk-2-1.png"))
  expect_true(all(fs::file_exists(figs_docs_v01)))
  # Update the file such that the previous 2 chunks are now named, plus add a
  # 3rd plot chunk
  fs::file_copy("files/test-wflow_build/figure-v02.Rmd", file_w_figs, overwrite = TRUE)
  build_v02 <- wflow_build(file_w_figs, view = FALSE, clean_fig_files = TRUE,
                           project = site_dir)
  expect_false(all(fs::file_exists(figs_analysis_v01)))
  expect_false(all(fs::file_exists(figs_docs_v01)))
  figs_analysis_v02 <- file.path(s$analysis, "figure", basename(file_w_figs),
                                 c("named1-1.png", "named2-1.png", "named3-1.png"))
  expect_false(all(fs::file_exists(figs_analysis_v02))) # moved by wflow_site()
  figs_docs_v02 <- file.path(s$docs, "figure", basename(file_w_figs),
                             c("named1-1.png", "named2-1.png", "named3-1.png"))
  expect_true(all(fs::file_exists(figs_docs_v02)))
  # Cleanup
  wflow_remove(file_w_figs, project = site_dir)
})

test_that("wflow_build does not remove unused figure files if clean_fig_files = FALSE", {

  skip_on_cran()

  # Build a file that has 2 plots from 2 unnamed chunks
  file_w_figs <- file.path(s$analysis, "fig.Rmd")
  fs::file_copy("files/test-wflow_build/figure-v01.Rmd", file_w_figs)
  build_v01 <- wflow_build(file_w_figs, view = FALSE, project = site_dir)
  figs_analysis_v01 <- file.path(s$analysis, "figure", basename(file_w_figs),
                                 c("unnamed-chunk-1-1.png", "unnamed-chunk-2-1.png"))
  expect_false(all(fs::file_exists(figs_analysis_v01))) # moved by wflow_site()
  figs_docs_v01 <- file.path(s$docs, "figure", basename(file_w_figs),
                             c("unnamed-chunk-1-1.png", "unnamed-chunk-2-1.png"))
  expect_true(all(fs::file_exists(figs_docs_v01)))
  # Update the file such that the previous 2 chunks are now named, plus add a
  # 3rd plot chunk
  fs::file_copy("files/test-wflow_build/figure-v02.Rmd", file_w_figs, overwrite = TRUE)
  build_v02 <- wflow_build(file_w_figs, view = FALSE, project = site_dir)
  expect_false(all(fs::file_exists(figs_analysis_v01)))

  # This line is the critical difference from the previous test. The outdated
  # figure files are still there since clean_fig_files = FALSE by default.
  expect_true(all(fs::file_exists(figs_docs_v01)))

  figs_analysis_v02 <- file.path(s$analysis, "figure", basename(file_w_figs),
                                 c("named1-1.png", "named2-1.png", "named3-1.png"))
  expect_false(all(fs::file_exists(figs_analysis_v02))) # moved by wflow_site()
  figs_docs_v02 <- file.path(s$docs, "figure", basename(file_w_figs),
                             c("named1-1.png", "named2-1.png", "named3-1.png"))
  expect_true(all(fs::file_exists(figs_docs_v02)))
  # Cleanup
  wflow_remove(file_w_figs, project = site_dir)
})

test_that("wflow_build deletes cache when delete_cache = TRUE", {

  skip_on_cran()

  # Build a file that has cached chunks
  file_w_cache <- file.path(s$analysis, "cache.Rmd")
  fs::file_copy("files/test-wflow_html/cache-all-chunks.Rmd", file_w_cache)
  build_v01 <- wflow_build(file_w_cache, view = FALSE, project = site_dir)
  dir_cache <- fs::path_ext_remove(file_w_cache)
  dir_cache <- glue::glue("{dir_cache}_cache")
  expect_true(fs::dir_exists(dir_cache))

  # By default, cache directory is not affected
  dir_cache_mod_pre <- fs::file_info(dir_cache)$modification_time
  expect_message(
    build_v02 <- wflow_build(file_w_cache, view = FALSE, project = site_dir),
    "  - Note: This file has a cache directory"
  )
  expect_false(build_v02$delete_cache)
  expect_true(fs::dir_exists(dir_cache))
  dir_cache_mod_post <- fs::file_info(dir_cache)$modification_time
  expect_equal(dir_cache_mod_post, dir_cache_mod_pre)

  # delete_cache deletes cache directory prior to building (it gets re-created)
  dir_cache_mod_pre <- fs::file_info(dir_cache)$modification_time
  expect_message(
    build_v03 <- wflow_build(file_w_cache, view = FALSE, delete_cache = TRUE,
                             project = site_dir),
    "  - Note: Deleted the cache directory before building"
  )
  expect_true(build_v03$delete_cache)
  expect_true(fs::dir_exists(dir_cache))
  dir_cache_mod_post <- fs::file_info(dir_cache)$modification_time
  expect_true(dir_cache_mod_post > dir_cache_mod_pre)

  # Cleanup
  wflow_remove(file_w_cache, project = site_dir)
})

test_that("wflow_build can display build log directly in R console with verbose", {

  skip_on_cran()

  x <- utils::capture.output(
    build <- wflow_build(rmd[2], view = FALSE, verbose = TRUE, project = site_dir))
  expect_true(build$verbose)
  expect_true(length(x) > 0)
})

test_that("wflow_build reports working and knit directories", {

  skip_on_cran()

  expect_message(
    wflow_build(rmd[2], view = FALSE, project = site_dir),
    getwd())

  expect_message(
    wflow_build(rmd[2], view = FALSE, project = site_dir),
    sprintf("Building %s in %s", rmd[2], site_dir))

  # Should not output knit directory if it's the same as working directory
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(site_dir)
  site_dir_new <- getwd()
  rmd_new <- "analysis/index.Rmd"

  expect_message(
    wflow_build(rmd_new, view = FALSE, project = site_dir),
    site_dir_new)

  expect_message(
    wflow_build(rmd_new, view = FALSE, project = site_dir),
    sprintf("Building %s", rmd_new))
})

# Test error handling ----------------------------------------------------------

test_that("wflow_build fails if file outside of analysis/", {
  rmd_outside <- file.path(s$root, "outside.Rmd")
  fs::file_create(rmd_outside)
  # When passing one invalid file
  expect_error(wflow_build(rmd_outside, project = site_dir),
               "Only files in the analysis directory can be built with wflow_build.")
  # When passing one invalid file with other valid files
  expect_error(wflow_build(c(rmd, rmd_outside), project = site_dir),
               "Only files in the analysis directory can be built with wflow_build.")
})

test_that("wflow_build fails early for bad files", {
  expect_error(wflow_build(character(), project = site_dir),
               "files must be NULL or a character vector of filenames")
  expect_error(wflow_build(s$analysis, project = site_dir),
               "files cannot include a path to a directory")
  expect_error(wflow_build("", project = site_dir),
               "Not all files exist. Check the paths to the files")
  fs::file_create(file.path(s$analysis, "invalid.R"))
  expect_error(wflow_build(file.path(s$analysis, "invalid.R"), project = site_dir),
               "File extensions must be either Rmd or rmd.")
})

test_that("wflow_build throws error if given directory input", {
  d <- file.path(site_dir, "toplevel")
  fs::dir_create(d)
  on.exit(unlink(d, recursive = TRUE, force = TRUE))
  expect_error(wflow_build(d, project = site_dir),
               "files cannot include a path to a directory")
})
