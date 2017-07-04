context("wflow_view")

# Setup ------------------------------------------------------------------------

# Setup workflowr project for testing
site_dir <- workflowr:::tempfile("test-wflow_view-", tmpdir = workflowr:::normalizePath("/tmp"))
suppressMessages(wflow_start(site_dir, change_wd = FALSE))
# Cleanup
on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))
p <- wflow_paths(project = site_dir)

# Create some fake R Markdown files
# Unfortunately cannot use wflow_open here b/c of devtools
rmd <- file.path(p$analysis, paste0(1:3, ".Rmd"))
for (i in 1:3) {
  file.copy("files/workflowr-template.Rmd", rmd[i])
}
# Expected html files
html <- workflowr:::to_html(rmd, outdir = p$docs)

# Build the site
suppressMessages(wflow_build(project = site_dir))

# Test wflow_view --------------------------------------------------------------

test_that("wflow_view opens docs/index.html by default.", {
  expected <- file.path(p$docs, "index.html")
  actual <- wflow_view(dry_run = TRUE, project = site_dir)
  expect_identical(actual, expected)
})

test_that("wflow_view can open most recently built HTML file.", {
  suppressMessages(wflow_build(rmd[1], project = site_dir))
  expected <- html[1]
  actual <- wflow_view(recent = TRUE, dry_run = TRUE, project = site_dir)
  expect_identical(actual, expected)
})

test_that("wflow_view can open a specific file.", {
  expected <- file.path(p$docs, "license.html")
  actual <- wflow_view(files = "license.html",
                       dry_run = TRUE, project = site_dir)
  expect_identical(actual, expected)
})

test_that("wflow_view can open multiple specific files.", {
  expected <- file.path(p$docs, c("license.html", "about.html"))
  actual <- wflow_view(files = c("license.html", "about.html"),
                       dry_run = TRUE, project = site_dir)
  expect_identical(actual, expected)
})

test_that("wflow_view can handle Rmd and html file extensions.", {
  expected <- file.path(p$docs, c("license.html", "about.html"))
  actual <- wflow_view(files = c("license.Rmd", "about.html"),
                       dry_run = TRUE, project = site_dir)
  expect_identical(actual, expected)
})

test_that("wflow_view ignores paths to files.", {
  expected <- file.path(p$docs, "about.html")
  actual <- wflow_view(files = "x/docs/about.html",
                       dry_run = TRUE, project = site_dir)
  expect_identical(actual, expected)
  actual <- wflow_view(files = "x/analysis/about.Rmd",
                       dry_run = TRUE, project = site_dir)
  expect_identical(actual, expected)
})

# Warnings and errors ----------------------------------------------------------

test_that("wflow_view sends warning for wrong file extension.", {
  expected <- file.path(p$docs, "about.html")
  expect_warning(actual <- wflow_view(files = c("about.html", "license.x"),
                       dry_run = TRUE, project = site_dir),
                 "The following files had invalid extensions and cannot be viewed:")
  expect_identical(actual, expected)
})


test_that("wflow_view sends warning for missing file.", {
  expected <- file.path(p$docs, "about.html")
  expect_warning(actual <- wflow_view(files = c("about.html", "missing.html"),
                                      dry_run = TRUE, project = site_dir),
                 "The following HTML files are missing:")
  expect_identical(actual, expected)
})

test_that("wflow_view throws error if no files to view.", {
  expect_error(suppressWarnings(wflow_view(files = "missing.html",
                                           dry_run = TRUE, project = site_dir)),
               "No HTML files were able to viewed.")
  expect_error(suppressWarnings(wflow_view(files = "missing.x",
                                           dry_run = TRUE, project = site_dir)),
               "None of the files had valid extensions.")
  unlink(file.path(p$docs, "index.html"))
  expect_error(suppressWarnings(wflow_view(dry_run = TRUE, project = site_dir)),
               "No HTML files were able to viewed.")
})
