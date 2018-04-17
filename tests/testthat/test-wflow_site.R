context("wflow_site")

# Test wflow_site --------------------------------------------------------------

test_that("wflow_site returns the correct output_dir", {
  tmp_dir <- tempfile()
  tmp_start <- wflow_start(tmp_dir, change_wd = FALSE, user.name = "Test Name",
                             user.email = "test@email")
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  observed <- wflow_site(file.path(tmp_dir, "analysis"))
  expect_identical(observed$output_dir, file.path(tmp_dir, "docs"))

  # Change output_dir to public/
  site_yml <- file.path(tmp_dir, "analysis", "_site.yml")
  site_yml_lines <- readLines(site_yml)
  site_yml_lines <- stringr::str_replace(site_yml_lines,
                                          "output_dir: \"../docs\"",
                                          "output_dir: \"../public\"")
  writeLines(site_yml_lines, site_yml)
  observed <- wflow_site(file.path(tmp_dir, "analysis"))
  expect_identical(observed$output_dir, file.path(tmp_dir, "public"))
  # get_output_dir() also creates the output directory
  expect_true(dir.exists(file.path(tmp_dir, "public")))
})

test_that("Passing a directory to wflow_site should build all Rmd files", {

  skip_on_cran()

  tmp_dir <- tempfile()
  tmp_start <- wflow_start(tmp_dir, change_wd = FALSE, user.name = "Test Name",
                             user.email = "test@email")
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  rmd <- Sys.glob(file.path(tmp_dir, "analysis", "*Rmd"))
  html <- workflowr:::to_html(rmd, outdir = file.path(tmp_dir, "docs"))

  observed <- render_site(file.path(tmp_dir, "analysis"), quiet = TRUE)
  expect_true(all(file.exists(html)))
})
