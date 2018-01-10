context("glob")

# Setup ------------------------------------------------------------------------

# start project in a tempdir
site_dir <- tempfile("test-glob-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE))
on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))
site_dir <- workflowr:::absolute(site_dir)
s <- wflow_status(project = site_dir)

rmd <- rownames(s$status)
stopifnot(length(rmd) > 0)
# Expected html files
html <- workflowr:::to_html(rmd, outdir = s$docs)

# Test file globbing -----------------------------------------------------------

test_that("wflow_build accepts file globs", {
  rmd_glob <- file.path(s$analysis, "*Rmd")
  build_w_glob <- wflow_build(rmd_glob, project = site_dir)
  expect_identical(Sys.glob(rmd_glob), build_w_glob$built)
  expect_error(wflow_build(file.path(s$analysis, "bad*blob.Rmd"), project = site_dir),
               "Not all files exist. Check the paths to the files")
})
