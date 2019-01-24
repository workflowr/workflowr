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
  site_yml_fname <- file.path(tmp_dir, "analysis", "_site.yml")
  site_yml <- yaml::yaml.load_file(site_yml_fname)
  site_yml$output_dir <- "../public"
  yaml::write_yaml(site_yml, file = site_yml_fname)

  observed <- wflow_site(file.path(tmp_dir, "analysis"))
  expect_identical(observed$output_dir, file.path(tmp_dir, "public"))
  # get_output_dir() also creates the output directory
  expect_true(fs::dir_exists(file.path(tmp_dir, "public")))
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
  expect_true(all(fs::file_exists(html)))
})

test_that("wflow_site copies CSS and JavaScript files to output directory", {

  skip_on_cran()

  tmp_dir <- tempfile()
  tmp_start <- wflow_start(tmp_dir, change_wd = FALSE, user.name = "Test Name",
                           user.email = "test@email")
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  p <- workflowr:::wflow_paths(project = tmp_dir)
  files_support <- c("style1.css", "style2.css", "script1.js", "script2.js")
  files_analysis <- file.path(p$analysis, files_support)
  files_docs <- file.path(p$docs, files_support)

  fs::file_create(files_analysis)
  render_site(p$analysis, quiet = TRUE)

  # CSS and JS files should have been copied (not moved) to docs directory
  expect_true(all(fs::file_exists(files_analysis)))
  expect_true(all(fs::file_exists(files_docs)))
})
