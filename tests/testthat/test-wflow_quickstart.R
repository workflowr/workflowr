context("wflow_quickstart")

test_that("wflow_quickstart copies Rmd into new project", {

  skip_on_cran()

  path <- fs::file_temp()
  on.exit(fs::dir_delete(path))
  rmd <- fs::file_temp(ext = ".Rmd")
  fs::file_create(rmd)
  on.exit(fs::file_delete(rmd), add = TRUE)
  withr::local_options(list(workflowr.view = FALSE))
  quick <- wflow_quickstart(files = rmd, username = "username",
                            directory = path, change_wd = FALSE,
                            create_on_github = FALSE,
                            git.user.name = "Test Name",
                            git.user.email = "test@email")
  rmd_new <- file.path(path, "analysis", fs::path_file(rmd))
  expect_true(fs::file_exists(rmd_new))
  html_new <- workflowr:::to_html(rmd_new, outdir = file.path(path, "docs"))
  expect_true(fs::file_exists(html_new))
})

test_that("wflow_quickstart uses the first Rmd to name the directory", {

  skip_on_cran()

  path <- fs::file_temp()
  fs::dir_create(path)
  withr::local_dir(path)
  on.exit(fs::dir_delete(path), add = TRUE)
  rmd <- c(fs::file_temp(ext = ".Rmd"), fs::file_temp(ext = ".Rmd"))
  fs::file_create(rmd)
  on.exit(fs::file_delete(rmd), add = TRUE)
  withr::local_options(list(workflowr.view = FALSE))
  quick <- wflow_quickstart(files = rmd, username = "username",
                            change_wd = FALSE,
                            create_on_github = FALSE,
                            git.user.name = "Test Name",
                            git.user.email = "test@email")
  rmd_new <- file.path(quick, "analysis", fs::path_file(rmd))
  expect_true(all(fs::file_exists(rmd_new)))
  html_new <- workflowr:::to_html(rmd_new, outdir = file.path(quick, "docs"))
  expect_true(all(fs::file_exists(html_new)))
  expect_identical(
    fs::path_file(quick),
    fs::path_ext_remove(fs::path_file(rmd[1]))
  )
})
