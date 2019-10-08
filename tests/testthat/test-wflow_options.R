context("wflow_options")

# Setup ------------------------------------------------------------------------

source("setup.R")

# Error handling ---------------------------------------------------------------

test_that("wflow_options sends a warning if knit_root_dir is abs path", {
  path <- test_setup()
  on.exit(test_teardown(path))

  p <- workflowr:::wflow_paths(project = path)

  wflow_yml <- file.path(p$root, "_workflowr.yml")
  wflow_yml_opts <- yaml::yaml.load_file(wflow_yml)
  wflow_yml_opts[["knit_root_dir"]] <- path
  yaml::write_yaml(wflow_yml_opts, file = wflow_yml)

  rmd <- file.path(p$analysis, "index.Rmd")
  expect_warning(workflowr:::wflow_options(rmd), "The value of knit_root_dir")

  skip_on_cran()

  expect_warning(wflow_build(rmd, view = FALSE, project = path),
                 "The value of knit_root_dir")
})
