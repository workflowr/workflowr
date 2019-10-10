context("wflow_quickstart")

test_that("wflow_quickstart copies Rmd into new project", {
  path <- fs::file_temp()
  rmd <- fs::file_temp(ext = ".Rmd")
  fs::file_create(rmd)
  withr::local_options(list(workflowr.view = FALSE))
  quick <- wflow_quickstart(files = rmd, username = "username",
                            directory = path, change_wd = FALSE,
                            create_on_github = FALSE)
  rmd_new <- file.path(path, "analysis", fs::path_file(rmd))
  expect_true(fs::file_exists(rmd_new))
  html_new <- workflowr:::to_html(rmd_new, outdir = file.path(path, "docs"))
  expect_true(fs::file_exists(html_new))
})
