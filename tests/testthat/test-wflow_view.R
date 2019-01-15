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
  fs::file_copy("files/example.Rmd", rmd[i])
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
  expect_identical(actual$opened, expected)
})

test_that("wflow_view can open most recently built HTML file.", {

  skip_on_cran()

  suppressMessages(wflow_build(rmd[1], view = FALSE, project = site_dir))
  expected <- html[1]
  actual <- wflow_view(latest = TRUE, dry_run = TRUE, project = site_dir)
  expect_identical(actual$opened, expected)
})

test_that("wflow_view can open a specific file.", {

  skip_on_cran()

  expected <- file.path(p$docs, "license.html")
  actual <- wflow_view(files = file.path(p$docs, "license.html"),
                       dry_run = TRUE, project = site_dir)
  expect_identical(actual$opened, expected)
})

test_that("wflow_view can open multiple specific files.", {

  skip_on_cran()

  expected <- file.path(p$docs, c("license.html", "about.html"))
  actual <- wflow_view(files = file.path(p$docs, c("license.html", "about.html")),
                       dry_run = TRUE, project = site_dir)
  expect_identical(actual$opened, expected)
})

test_that("wflow_view can handle Rmd and html file extensions.", {

  skip_on_cran()

  expected <- file.path(p$docs, c("license.html", "about.html"))
  actual <- wflow_view(files = c(file.path(p$analysis, "license.Rmd"),
                                 file.path(p$docs, "about.html")),
                       dry_run = TRUE, project = site_dir)
  expect_identical(actual$opened, expected)
})

test_that("wflow_view can open most recently built HTML file in addition to other files.", {

  skip_on_cran()

  suppressMessages(wflow_build(rmd[1], view = FALSE, project = site_dir))
  expected <- c(html[2:1])
  actual <- wflow_view(files = rmd[2], latest = TRUE, dry_run = TRUE,
                       project = site_dir)
  expect_identical(actual$opened, expected)

  # Shouldn't be redundant
  expected <- c(html[1:2])
  actual <- wflow_view(files = rmd[1:2], latest = TRUE, dry_run = TRUE,
                       project = site_dir)
  expect_identical(actual$opened, expected)
})

test_that("wflow_view requires correct paths to files.", {

  skip_on_cran()

  expect_error(wflow_view(files = "x/docs/about.html",
                          dry_run = TRUE, project = site_dir),
               "Not all files exist. Check the paths to the files")

  expect_error(wflow_view(files = "x/analysis/about.Rmd",
                          dry_run = TRUE, project = site_dir),
               "Not all files exist. Check the paths to the files")
})

test_that("wflow_view removes duplicates.", {

  skip_on_cran()

  expected <- html
  actual <- wflow_view(files = c(html, html), dry_run = TRUE, project = site_dir)
  expect_identical(actual$opened, expected)
  actual <- wflow_view(files = c(html, html, rmd), dry_run = TRUE, project = site_dir)
  expect_identical(actual$opened, expected)
})

# Warnings and errors ----------------------------------------------------------

test_that("wflow_view throws error for wrong file extension.", {
  expect_error(wflow_view(files = file.path(p$analysis, "_site.yml"),
                          dry_run = TRUE, project = site_dir),
               "File extensions must be either Rmd, rmd, or html.")
})

test_that("wflow_view sends warning for missing HTML file.", {

  skip_on_cran()

  rmd_wo_html <- file.path(p$analysis, "rmd_wo_html.Rmd")
  on.exit(fs::file_delete(rmd_wo_html))
  fs::file_create(rmd_wo_html)

  expected <- file.path(p$docs, "about.html")
  expect_warning(actual <- wflow_view(files = c(file.path(p$docs, "about.html"),
                                                rmd_wo_html),
                                      dry_run = TRUE, project = site_dir),
                 "The following HTML files are missing:")
  expect_identical(actual$opened, expected)
})

test_that("wflow_view throws error if no files to view.", {

  rmd_wo_html <- file.path(p$analysis, "rmd_wo_html.Rmd")
  on.exit(fs::file_delete(rmd_wo_html))
  fs::file_create(rmd_wo_html)

  expect_error(suppressWarnings(wflow_view(files = rmd_wo_html,
                                           dry_run = TRUE, project = site_dir)),
               "No HTML files were able to viewed.")

  unlink(file.path(p$docs, "index.html"))
  expect_error(suppressWarnings(wflow_view(dry_run = TRUE, project = site_dir)),
               "No HTML files were able to viewed.")
})

test_that("wflow_view throws error if given directory input.", {
  d <- file.path(site_dir, "toplevel")
  fs::dir_create(d)
  on.exit(unlink(d, recursive = TRUE, force = TRUE))
  expect_error(wflow_view(d, project = site_dir),
               "files cannot include a path to a directory")
})

test_that("wflow_view throws error if given non-workflowr files.", {

  invalid <- list(rmd_in_root = file.path(p$root, "file.Rmd"),
                  rmd_in_docs = file.path(p$docs, "file.Rmd"),
                  html_in_root = file.path(p$root, "file.html"),
                  html_in_analysis = file.path(p$analysis, "file.html"))
  on.exit(lapply(invalid, fs::file_delete))
  lapply(invalid, fs::file_create)

  for (f in invalid) {
    expect_error(wflow_view(f, dry_run = TRUE, project = site_dir))
  }
})



test_that("wflow_view S3 print method", {

  skip_on_cran()

  # Have to ensure that getOption("browser") is set since this changes the print
  # method output. The value for the option can be a string or a function
  withr::with_options(list(browser = "firefox"),
                      {
                        actual <- utils::capture.output(
                          wflow_view(html[1], dry_run = TRUE, project = site_dir))
                        expect_true(length(actual) == 2)
                        expect_identical(actual[1], "wflow_view would open:")
                        expect_true(stringr::str_detect(actual[2], html[1]))
                      })

  withr::with_options(list(browser = function(x) x),
                      {
                        actual <- utils::capture.output(
                          wflow_view(html[1], dry_run = TRUE, project = site_dir))
                        expect_true(length(actual) == 2)
                        expect_identical(actual[1], "wflow_view would open:")
                        expect_true(stringr::str_detect(actual[2], html[1]))
                      })

})

test_that("wflow_view reports if browser does not exist", {

  skip_on_cran()

  withr::with_options(list(browser = ""),
                     {
                       expect_silent(actual <-
                                       wflow_view(html[1], dry_run = TRUE,
                                                  project = site_dir))
                       expect_false(actual$browser)
                       print_method <- utils::capture.output(actual)
                       expect_true(any(stringr::str_detect(print_method,
                                                           "\\?browseURL")))
                     })

  withr::with_options(list(browser = NULL),
                      {
                        expect_silent(actual <-
                                        wflow_view(html[1], dry_run = TRUE,
                                                   project = site_dir))
                        expect_false(actual$browser)
                        print_method <- utils::capture.output(actual)
                        expect_true(any(stringr::str_detect(print_method,
                                                            "\\?browseURL")))
                      })
})
