context("suppress_report")

source("setup.R")

skip_on_cran_windows()

test_that("suppress_report is FALSE by default", {
  path <- fs::file_temp()
  test_setup(path)
  on.exit(test_teardown(path))

  wflow_opts <- workflowr:::wflow_options(file.path(path, "index.Rmd"))

  expect_false(wflow_opts$suppress_report)
})

test_that("suppress_report generates a report that is an empty string", {
  path <- fs::file_temp()
  test_setup(path)
  on.exit(test_teardown(path))

  s <- wflow_status(project = path)
  input <- rownames(s$status)[1]
  output_dir <- s$docs
  has_code <- FALSE
  wflow_opts <- workflowr:::wflow_options(input)

  report <- create_report(input = input, output_dir = output_dir,
                          has_code = has_code, opts = wflow_opts)
  expect_false(identical(report, ""))

  wflow_opts$suppress_report <- TRUE

  report <- create_report(input = input, output_dir = output_dir,
                          has_code = has_code, opts = wflow_opts)
  expect_true(identical(report, ""))
})

test_that("suppress_report in _workflowr.yml applies to all Rmd files", {
  path <- fs::file_temp()
  test_setup(path)
  on.exit(test_teardown(path))

  wflow_yml <- file.path(path, "_workflowr.yml")
  cat("suppress_report: TRUE\n", file = wflow_yml, append = TRUE)

  wflow_opts <- workflowr:::wflow_options(file.path(path, "index.Rmd"))
  expect_true(wflow_opts$suppress_report)

  skip_on_cran()

  o <- wflow_publish(files = file.path(path, "analysis", "*Rmd"), view = FALSE,
                     project = path)

  for (html in o$step2$html) {
    lines <- readLines(html)
    expect_false(any(stringr::str_detect(lines, "workflowr-report")))
  }
})

test_that("suppress_report in YAML header applies to specific Rmd file", {
  path <- fs::file_temp()
  test_setup(path)
  on.exit(test_teardown(path))

  index <- file.path(path, "analysis", "index.Rmd")
  rmd <- file.path(path, "analysis", "file.Rmd")
  lines <- c("---",
             "output: workflowr::wflow_html",
             "workflowr:",
             "  suppress_report: TRUE",
             "---",
             "",
             "`r 1 + 1`")
  writeLines(lines, rmd)

  expect_false(workflowr:::wflow_options(index)$suppress_report)
  expect_true(workflowr:::wflow_options(rmd)$suppress_report)


  skip_on_cran()

  o <- wflow_build(c(index, rmd), view = FALSE, project = path)

  index_html <- workflowr:::to_html(index, outdir = file.path(path, "docs"))
  index_html_lines <- readLines(index_html)
  expect_true(any(stringr::str_detect(index_html_lines, "workflowr-report")))

  html <- workflowr:::to_html(rmd, outdir = file.path(path, "docs"))
  html_lines <- readLines(html)
  expect_false(any(stringr::str_detect(html_lines, "workflowr-report")))
})

test_that("suppress_report can also be set to yes and no", {
  # This is because of the behavior of yaml::yaml.load_file() and yaml::write_yaml()

  path <- fs::file_temp()
  test_setup(path)
  on.exit(test_teardown(path))

  wflow_yml <- file.path(path, "_workflowr.yml")
  cat("suppress_report: yes\n", file = wflow_yml)
  wflow_opts <- workflowr:::wflow_options(file.path(path, "index.Rmd"))
  expect_true(wflow_opts$suppress_report)

  cat("suppress_report: no\n", file = wflow_yml)
  wflow_opts <- workflowr:::wflow_options(file.path(path, "index.Rmd"))
  expect_false(wflow_opts$suppress_report)
})
