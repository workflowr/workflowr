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
