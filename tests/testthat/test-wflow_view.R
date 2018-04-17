context("wflow_view")

# Setup ------------------------------------------------------------------------

# Setup workflowr project for testing
site_dir <- tempfile("test-wflow_view-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE, user.name = "Test Name",
                             user.email = "test@email"))
# Cleanup
on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))
site_dir <- workflowr:::absolute(site_dir)
p <- wflow_paths(project = site_dir)

# Create some fake R Markdown files
rmd <- file.path(p$analysis, paste0(1:3, ".Rmd"))
for (i in 1:3) {
  file.copy("files/example.Rmd", rmd[i])
}
# Expected html files
html <- workflowr:::to_html(rmd, outdir = p$docs)


# Test wflow_view --------------------------------------------------------------

test_that("wflow_view opens docs/index.html by default.", {

  skip_on_cran()

  # Build the site
  suppressMessages(wflow_build(view = FALSE, project = site_dir))

  expected <- file.path(p$docs, "index.html")
  actual <- wflow_view(dry_run = TRUE, project = site_dir)
  expect_identical(actual, expected)
})

test_that("wflow_view can open most recently built HTML file.", {

  skip_on_cran()

  suppressMessages(wflow_build(rmd[1], view = FALSE, project = site_dir))
  expected <- html[1]
  actual <- wflow_view(recent = TRUE, dry_run = TRUE, project = site_dir)
  expect_identical(actual, expected)
})

test_that("wflow_view can open a specific file.", {

  skip_on_cran()

  expected <- file.path(p$docs, "license.html")
  actual <- wflow_view(files = "license.html",
                       dry_run = TRUE, project = site_dir)
  expect_identical(actual, expected)
})

test_that("wflow_view can open multiple specific files.", {

  skip_on_cran()

  expected <- file.path(p$docs, c("license.html", "about.html"))
  actual <- wflow_view(files = c("license.html", "about.html"),
                       dry_run = TRUE, project = site_dir)
  expect_identical(actual, expected)
})

test_that("wflow_view can handle Rmd and html file extensions.", {

  skip_on_cran()

  expected <- file.path(p$docs, c("license.html", "about.html"))
  actual <- wflow_view(files = c("license.Rmd", "about.html"),
                       dry_run = TRUE, project = site_dir)
  expect_identical(actual, expected)
})

test_that("wflow_view ignores paths to files.", {

  skip_on_cran()

  expected <- file.path(p$docs, "about.html")
  actual <- wflow_view(files = "x/docs/about.html",
                       dry_run = TRUE, project = site_dir)
  expect_identical(actual, expected)
  actual <- wflow_view(files = "x/analysis/about.Rmd",
                       dry_run = TRUE, project = site_dir)
  expect_identical(actual, expected)
})

# Warnings and errors ----------------------------------------------------------

test_that("wflow_view throws error for wrong file extension.", {
  expect_error(wflow_view(files = c("about.html", "license.x"),
                          dry_run = TRUE, project = site_dir),
               "File extensions must be either Rmd, rmd, or html.")
})

test_that("wflow_view sends warning for missing file.", {

  skip_on_cran()

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
               "File extensions must be either Rmd, rmd, or html.")
  unlink(file.path(p$docs, "index.html"))
  expect_error(suppressWarnings(wflow_view(dry_run = TRUE, project = site_dir)),
               "No HTML files were able to viewed.")
})

test_that("wflow_view throws error if given directory input.", {
  d <- file.path(site_dir, "toplevel")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE, force = TRUE))
  expect_error(wflow_view(d, project = site_dir),
               "files cannot include a path to a directory")
})
