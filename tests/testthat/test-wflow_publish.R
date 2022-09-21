context("wflow_publish")

# Setup ------------------------------------------------------------------------

library("git2r")

# Setup workflowr project for testing
site_dir <- tempfile("test-wflow_publish-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE, user.name = "Test Name",
                             user.email = "test@email"))
# Delete workflowr project on exit
on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))
site_dir <- workflowr:::absolute(site_dir)
s <- wflow_status(project = site_dir)
r <- repository(s$root)

rmd <- file.path(s$analysis, c("about.Rmd", "index.Rmd", "license.Rmd"))
html <- workflowr:::to_html(rmd, outdir = s$docs)

rmd_to_fail <- file.path(s$analysis, "error.Rmd")
fs::file_copy("files/test-wflow_build/error.Rmd", rmd_to_fail)

# Load helper function local_no_gitconfig()
source("helpers.R", local = TRUE)

# Test wflow_publish -----------------------------------------------------------

test_that("wflow_publish works in a simple case", {

  skip_on_cran()

  expect_message(o <- wflow_publish(rmd, view = FALSE, project = site_dir),
                 rmd[1])
  expect_true(all(fs::file_exists(html)))
  s <- wflow_status(project = site_dir)
  expect_true(all(s$status[rmd, "published"]))
})

# Create decoy file that should not be built since it is unpublished
rmd_decoy <- file.path(s$analysis, "decoy.Rmd")
fs::file_create(rmd_decoy)
html_decoy <- workflowr:::to_html(rmd_decoy, outdir = s$docs)

test_that("wflow_publish can `republish`", {

  skip_on_cran()

  mtime_pre <- file.mtime(html)
  Sys.sleep(2)
  # Change the theme
  config <- file.path(s$analysis, "_site.yml")
  config_lines <- readLines(config)
  config_lines <- stringr::str_replace(config_lines,
                                       "    theme: cosmo",
                                       "    theme: readable")
  writeLines(config_lines, con = config)
  # Republish with new theme
  expect_message(o <- wflow_publish(config, republish = TRUE, view = FALSE,
                                    project = site_dir),
                 rmd[1])
  mtime_post <- file.mtime(html)
  expect_true(all(mtime_post > mtime_pre))
  expect_true(config == o$step1$commit_files)
  expect_true(all(html %in% o$step3$commit_files))
  expect_false(fs::file_exists(html_decoy))
  expect_false(html_decoy %in% o$step3$commit_files)
})

# Commit decoy file. Should not be affected by `update = TRUE` b/c it has not
# been published.
wflow_git_commit(rmd_decoy, "Commit decoy Rmd", project = site_dir)

test_that("wflow_publish can `update`", {

  skip_on_cran()

  # Edit and manually commit a published Rmd file, then use `update` to publish.
  cat("edit", file = rmd[1], append = TRUE)
  wflow_git_commit(rmd[1], "Draft edit", project = site_dir)
  # Update
  expect_message(o <- wflow_publish(update = TRUE, view = FALSE, project = site_dir),
                 rmd[1])
  expect_true(is.null(o$step1))
  expect_true(html[1] == o$step3$commit_files)
  expect_false(fs::file_exists(html_decoy))
  expect_false(html_decoy %in% o$step3$commit_files)
})

test_that("wflow_publish does not republish files with unstaged/staged changes", {

  skip_on_cran()

  # republish = TRUE builds and commits all previously published files. However,
  # if the files are not committed in step1, then they need to be skipped to
  # avoid getting out of sync.

  commit_head <- commits(r)[[1]]
  on.exit(reset(commit_head, reset_type = "hard"))

  # about.Rmd should be skipped b/c it has unstaged changes
  cat("edit", file = rmd[1], append = TRUE)
  # index.Rmd should be skipped b/c it has staged changes
  cat("edit", file = rmd[2], append = TRUE)
  add(r, rmd[2])
  # Republish should only build license.Rmd
  expect_message(o <- wflow_publish(republish = TRUE, view = FALSE, project = site_dir),
                 rmd[3])
  expect_true(is.null(o$step1))
  expect_true(rmd[3] == o$step2$built)
  # license.html and index.Rmd (b/c it was staged) are the only commit_files
  expect_false(html[1] %in% o$step3$commit_files)
  expect_false(html[2] %in% o$step3$commit_files)
  expect_true(html[3] %in% o$step3$commit_files)
})

test_that("wflow_publish can be used to commit non-Rmd files instead of wflow_git_commit", {

  skip_on_cran()

  f_test <- file.path(s$root, "test.txt")
  fs::file_create(f_test)
  expect_silent(o <- wflow_publish(f_test, view = FALSE, project = site_dir))
  expect_true(f_test == o$step1$commit_files)
  expect_true(is.null(o$step2))
  expect_true(is.null(o$step3))
})

test_that("wflow_publish automatically removes unused figure files", {

  skip_on_cran()

  # Publish a file that has 2 plots from 2 unnamed chunks
  file_w_figs <- file.path(s$analysis, "fig.Rmd")
  fs::file_copy("files/test-wflow_build/figure-v01.Rmd", file_w_figs)
  publish_v01 <- wflow_publish(file_w_figs, view = FALSE, project = site_dir)
  figs_analysis_v01 <- file.path(s$analysis, "figure", basename(file_w_figs),
                                 c("unnamed-chunk-1-1.png", "unnamed-chunk-2-1.png"))
  expect_false(all(fs::file_exists(figs_analysis_v01))) # moved by wflow_site()
  figs_docs_v01 <- file.path(s$docs, "figure", basename(file_w_figs),
                             c("unnamed-chunk-1-1.png", "unnamed-chunk-2-1.png"))
  expect_true(all(fs::file_exists(figs_docs_v01)))
  expect_true(all(figs_docs_v01 %in% publish_v01$step3$commit_files))
  # Update the file such that the previous 2 chunks are now named, plus add a
  # 3rd plot chunk
  fs::file_copy("files/test-wflow_build/figure-v02.Rmd", file_w_figs, overwrite = TRUE)
  publish_v02 <- wflow_publish(file_w_figs, view = FALSE, project = site_dir)
  expect_false(all(fs::file_exists(figs_analysis_v01)))
  expect_false(all(fs::file_exists(figs_docs_v01)))
  figs_analysis_v02 <- file.path(s$analysis, "figure", basename(file_w_figs),
                                 c("named1-1.png", "named2-1.png", "named3-1.png"))
  expect_false(all(fs::file_exists(figs_analysis_v02))) # moved by wflow_site()
  figs_docs_v02 <- file.path(s$docs, "figure", basename(file_w_figs),
                             c("named1-1.png", "named2-1.png", "named3-1.png"))
  expect_true(all(fs::file_exists(figs_docs_v02)))
  expect_true(all(figs_docs_v02 %in% publish_v02$step3$commit_files))
  # The v01 files should also be listed in the commit_files b/c they are removed
  # in this commit
  expect_true(all(figs_docs_v01 %in% publish_v02$step3$commit_files))
  # The Git status should have no staged or unstaged changes, which would occur
  # if the files were deleted but not committed
  current_status <- status(r)
  expect_false(length(current_status$staged) > 0)
  expect_false(length(current_status$unstaged) > 0)
  # Cleanup
  wflow_remove(file_w_figs, project = site_dir)
})

# This tests the edge case where a file had one or more figures but then gets
# reduced to zero. While Git is able to "add" a non-existent directory to stage
# deleted files, git2r chokes if the non-existent directory is a relative path.
# git2r requires the non-existent directory to either be an absolute path or a
# relative path from the root of the Git repo.
test_that("wflow_publish removes unused figure files even if directory no longer exists", {

  skip_on_cran()

  # Publish a file that has 2 plots from 2 unnamed chunks
  file_w_figs <- file.path(s$analysis, "fig.Rmd")
  fs::file_copy("files/test-wflow_build/figure-v01.Rmd", file_w_figs)
  publish_v01 <- wflow_publish(file_w_figs, view = FALSE, project = site_dir)
  figs_analysis_v01 <- file.path(s$analysis, "figure", basename(file_w_figs),
                                 c("unnamed-chunk-1-1.png", "unnamed-chunk-2-1.png"))
  # expect_true(all(fs::file_exists(figs_analysis_v01))) # see wflow_site()
  figs_docs_v01 <- file.path(s$docs, "figure", basename(file_w_figs),
                             c("unnamed-chunk-1-1.png", "unnamed-chunk-2-1.png"))
  expect_true(all(fs::file_exists(figs_docs_v01)))
  expect_true(all(figs_docs_v01 %in% publish_v01$step3$commit_files))
  # Update the file to have no plots
  fs::file_copy("files/test-wflow_build/seed.Rmd", file_w_figs, overwrite = TRUE)
  publish_v02 <- wflow_publish(file_w_figs, view = FALSE, project = site_dir)
  expect_false(all(fs::file_exists(figs_analysis_v01)))
  expect_false(all(fs::file_exists(figs_docs_v01)))
  # The old figure files should also be listed in the commit_files b/c they are
  # removed in this commit
  expect_true(all(figs_docs_v01 %in% publish_v02$step3$commit_files))
  # The Git status should have no staged or unstaged changes, which would occur
  # if the files were deleted but not committed
  current_status <- status(r)
  expect_false(length(current_status$staged) > 0)
  expect_false(length(current_status$unstaged) > 0)
  # Cleanup
  wflow_remove(file_w_figs, project = site_dir)
})

test_that("wflow_publish deletes cache when delete_cache = TRUE", {

  skip_on_cran()
  skip_on_os("windows") # Avoid errors due to long filenames

  # Build a file that has cached chunks
  file_w_cache <- file.path(s$analysis, "cache.Rmd")
  fs::file_copy("files/test-wflow_html/cache-all-chunks.Rmd", file_w_cache)
  publish_v01 <- wflow_publish(file_w_cache, view = FALSE, project = site_dir)
  dir_cache <- fs::path_ext_remove(file_w_cache)
  dir_cache <- glue::glue("{dir_cache}_cache")
  expect_true(fs::dir_exists(dir_cache))

  # By default, cache directory is not affected
  dir_cache_mod_pre <- fs::file_info(dir_cache)$modification_time
  expect_message(
    publish_v02 <- wflow_publish(file_w_cache, view = FALSE, project = site_dir),
    "  - Note: This file has a cache directory"
  )
  expect_false(publish_v02$step2$delete_cache)
  expect_true(fs::dir_exists(dir_cache))
  dir_cache_mod_post <- fs::file_info(dir_cache)$modification_time
  expect_equal(dir_cache_mod_post, dir_cache_mod_pre)

  # delete_cache deletes cache directory prior to building (it gets re-created)
  dir_cache_mod_pre <- fs::file_info(dir_cache)$modification_time
  expect_message(
    publish_v03 <- wflow_publish(file_w_cache, view = FALSE, delete_cache = TRUE,
                                 project = site_dir),
    "  - Note: Deleted the cache directory before building"
  )
  expect_true(publish_v03$step2$delete_cache)
  expect_true(fs::dir_exists(dir_cache))
  dir_cache_mod_post <- fs::file_info(dir_cache)$modification_time
  expect_true(dir_cache_mod_post > dir_cache_mod_pre)

  # Cleanup
  wflow_remove(file_w_cache, project = site_dir)
})

test_that("wflow_publish commits CSS/JavaScript files", {

  skip_on_cran()

  files_support <- c("style1.css", "style2.css", "script1.js", "script2.js")
  files_analysis <- file.path(s$analysis, files_support)
  files_docs <- file.path(s$docs, files_support)

  fs::file_create(files_analysis)
  rmd <- file.path(s$analysis, "index.Rmd")
  o <- wflow_publish(c(rmd, files_analysis), view = FALSE, project = site_dir)

  # CSS and JS files should have been copied (not moved) to docs directory
  expect_true(all(fs::file_exists(files_analysis)))
  expect_true(all(fs::file_exists(files_docs)))
  # CSS and JS files should have been committed in step 3
  expect_true(all(files_docs %in% o$step3$commit_files))
})

test_that("wflow_publish commits new .nojekyll after docs/ name change", {

  skip_on_cran()

  x <- wflow_start(tempfile(), change_wd = FALSE, user.name = "Test Name",
                   user.email = "test@email")
  p <- workflowr:::wflow_paths(project = x$directory)
  site_yml <- file.path(p$analysis, "_site.yml")
  y <- yaml::yaml.load_file(site_yml)
  y$output_dir <- "../test"
  yaml::write_yaml(y, file = site_yml)
  # Create the new output directory. Otherwise receive multiple warnings. I
  # should improve this (added to project Improvements)
  fs::dir_create(file.path(x$directory, "test"))
  publish <- wflow_publish(c(site_yml, file.path(p$analysis, "*Rmd")),
                           "Change output dir to test/", view = FALSE,
                           project = x$directory)
  nojekyll <- file.path(p$root, "test", ".nojekyll")
  expect_true(fs::file_exists(nojekyll))
  expect_true(nojekyll %in% publish$step3$commit_files)
})

test_that("wflow_publish can display build log directly in R console with verbose", {

  skip_on_cran()

  x <- utils::capture.output(
    publish <- wflow_publish(rmd[2], view = FALSE, verbose = TRUE, project = site_dir))
  expect_true(publish$step2$verbose)
  expect_true(length(x) > 0)
})

test_that("wflow_build can combine files to build using the intersection of the provided args", {

  skip_on_cran()

  status <- wflow_status(project = site_dir)
  rmd_published <- rownames(status$status)[status$status$published]

  # With combine = "or" (default), build `files` and `republish`
  expect_identical(
    wflow_publish(rmd_decoy, republish = TRUE, dry_run = TRUE, project = site_dir)$step2$built,
    c(rmd_decoy, rmd_published)
  )
  # With combine = "and", only build intersection of `files` and `republish`
  expect_null(
    wflow_publish(rmd_decoy, republish = TRUE, combine = "and", dry_run = TRUE, project = site_dir)$step2$built
  )
  expect_identical(
    wflow_publish(rmd_published[2], republish = TRUE, combine = "and", dry_run = TRUE, project = site_dir)$step2$built,
    rmd_published[2]
  )
})

# Test error handling ----------------------------------------------------------

test_that("wflow_publish resets Git repo to previous commit if build fails", {

  skip_on_cran()

  commit_pre <- commits(r, n = 1)[[1]]
  expect_error(utils::capture.output(
    wflow_publish(rmd_to_fail, view = FALSE, project = site_dir)),
               "There was an error")
  commit_post <- commits(r, n = 1)[[1]]
  expect_identical(commit_post, commit_pre)
})

test_that("wflow_publish restores previous docs/ if build fails", {

  skip_on_cran()

  md5sum_pre <- tools::md5sum(html)
  mtime_pre <- file.mtime(html)
  Sys.sleep(2)
  expect_error(utils::capture.output(
    wflow_publish(c(rmd, rmd_to_fail), view = FALSE, project = site_dir)),
    "There was an error")
  md5sum_post <- tools::md5sum(html)
  mtime_post <- file.mtime(html)
  expect_identical(md5sum_post, md5sum_pre)
  expect_equal(mtime_post, mtime_pre)
  expect_true(all(mtime_post - mtime_pre < 0.1))
})

test_that("wflow_publish does *not* backup docs/ if it doesn't exist", {

  skip_on_cran()

  docs <- file.path(site_dir, "docs")
  docs_tmp <- fs::file_temp("docs-")
  on.exit(file.rename(docs_tmp, docs))
  file.rename(docs, docs_tmp)
  # It should no longer send a Warning about the directory not existing
  expect_silent(suppressMessages(
    published <- wflow_publish(rmd, view = FALSE, project = site_dir)
  ))
  # Remove the docs/ that was just created so that it can be restored on exit
  unlink(docs, recursive = TRUE, force = TRUE)
})

test_that("wflow_publish throws an error if user.name and user.email are not set", {

  skip_on_cran()

  # local_no_gitconfig() is defined in tests/testthat/helpers.R
  local_no_gitconfig("-workflowr")

  # Also have to remove local ./.git/config in the project's Git repo. Couldn't
  # figure out a good way to do this with withr. Couldn't get to "restore"
  # function to run at the end of the function call.
  gitconfig <- file.path(site_dir, ".git", "config")
  gitconfig_tmp <- file.path(tempdir(), "config")
  file.rename(gitconfig, gitconfig_tmp)
  on.exit(file.rename(gitconfig_tmp, gitconfig), add = TRUE)

  expect_error(wflow_publish(project = site_dir),
               "You must set your user.name and user.email for Git first")
  expect_error(wflow_publish(project = site_dir),
               "wflow_publish")
})

test_that("wflow_publish throws an error if there are no files to publish", {

  skip_on_cran()

  # Note: Have to escape the parentheses for the regex to match
  expect_error(wflow_publish(project = site_dir),
               "You did not tell wflow_publish\\(\\) what to publish.")
})

test_that("wflow_publish throws error if combine=and but no files specified", {

  expect_error(wflow_publish(combine = "and", dry_run = TRUE, project = site_dir),
               "can only be used when explicitly specifying Rmd files")

  expect_error(wflow_publish(republish = TRUE, combine = "and", dry_run = TRUE,
                             project = site_dir),
               "can only be used when explicitly specifying Rmd files")
})
