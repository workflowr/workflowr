context("wflow_quickstart")

# Make sure the temporary directory doesn't contain any Rmd files
local({
  rmd <- fs::dir_ls(path = fs::path_temp(), glob = "*Rmd")
  fs::file_delete(rmd)
})

test_that("wflow_quickstart copies Rmd into new project", {

  skip_on_cran()

  path <- workflowr:::absolute(fs::file_temp())
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

  path <- workflowr:::absolute(fs::file_temp())
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

test_that("wflow_quickstart can change working directory", {

  skip_on_cran()

  cwd <- getwd()
  on.exit(setwd(cwd), add = TRUE)
  path <- workflowr:::absolute(fs::file_temp())
  on.exit(fs::dir_delete(path), add = TRUE)
  rmd <- fs::file_temp(ext = ".Rmd")
  fs::file_create(rmd)
  on.exit(fs::file_delete(rmd), add = TRUE)
  withr::local_options(list(workflowr.view = FALSE))
  quick <- wflow_quickstart(files = rmd, username = "username",
                            directory = path, change_wd = TRUE,
                            create_on_github = FALSE,
                            git.user.name = "Test Name",
                            git.user.email = "test@email")
  expect_identical(getwd(), as.character(path))
})

test_that("wflow_quickstart copies and commits supporting file", {

  skip_on_cran()

  path <- workflowr:::absolute(fs::file_temp())
  on.exit(fs::dir_delete(path))
  rmd <- fs::file_temp(ext = ".Rmd")
  fs::file_create(rmd)
  on.exit(fs::file_delete(rmd), add = TRUE)
  support <- fs::file_temp(ext = ".R")
  fs::file_create(support)
  on.exit(fs::file_delete(support), add = TRUE)

  withr::local_options(list(workflowr.view = FALSE))
  quick <- wflow_quickstart(files = rmd, username = "username",
                            supporting_files = support,
                            directory = path, change_wd = FALSE,
                            create_on_github = FALSE,
                            git.user.name = "Test Name",
                            git.user.email = "test@email")
  support_new <- file.path(path, fs::path_file(support))
  expect_true(fs::file_exists(support_new))
  committed_files <- workflowr:::get_committed_files(git2r::repository(path))
  expect_true(support_new %in% committed_files)
})

test_that("wflow_quickstart accepts file globs for Rmd files", {

  skip_on_cran()

  path <- workflowr:::absolute(fs::file_temp())
  on.exit(fs::dir_delete(path))
  rmd <- fs::file_temp(pattern = letters[1:3], ext = ".Rmd")
  fs::file_create(rmd)
  on.exit(fs::file_delete(rmd), add = TRUE)
  withr::local_options(list(workflowr.view = FALSE))
  quick <- wflow_quickstart(files = file.path(fs::path_temp(), "*Rmd"),
                            username = "username",
                            directory = path, change_wd = FALSE,
                            create_on_github = FALSE,
                            git.user.name = "Test Name",
                            git.user.email = "test@email")
  rmd_new <- file.path(path, "analysis", fs::path_file(rmd))
  expect_true(all(fs::file_exists(rmd_new)))
  html_new <- workflowr:::to_html(rmd_new, outdir = file.path(path, "docs"))
  expect_true(all(fs::file_exists(html_new)))
})

test_that("wflow_quickstart accepts file globs for supporting files", {

  skip_on_cran()

  path <- workflowr:::absolute(fs::file_temp())
  on.exit(fs::dir_delete(path))
  rmd <- fs::file_temp(ext = ".Rmd")
  fs::file_create(rmd)
  on.exit(fs::file_delete(rmd), add = TRUE)
  support <- fs::file_temp(pattern = letters[1:3], ext = ".R")
  fs::file_create(support)
  on.exit(fs::file_delete(support), add = TRUE)

  withr::local_options(list(workflowr.view = FALSE))
  quick <- wflow_quickstart(files = rmd, username = "username",
                            supporting_files = file.path(fs::path_temp(), "*R"),
                            directory = path, change_wd = FALSE,
                            create_on_github = FALSE,
                            git.user.name = "Test Name",
                            git.user.email = "test@email")
  support_new <- file.path(path, fs::path_file(support))
  expect_true(all(fs::file_exists(support_new)))
  committed_files <- workflowr:::get_committed_files(git2r::repository(path))
  expect_true(all(support_new %in% committed_files))
})

test_that("wflow_quickstart copies and commits supporting directories", {

  skip_on_cran()

  path <- workflowr:::absolute(fs::file_temp())
  on.exit(fs::dir_delete(path))
  rmd <- fs::file_temp(ext = ".Rmd")
  fs::file_create(rmd)
  on.exit(fs::file_delete(rmd), add = TRUE)
  support_dir <- fs::file_temp()
  fs::dir_create(support_dir)
  on.exit(fs::file_delete(support_dir), add = TRUE)
  support <- file.path(support_dir, paste0(letters[1:3], ".R"))
  fs::file_create(support)

  withr::local_options(list(workflowr.view = FALSE))
  quick <- wflow_quickstart(files = rmd, username = "username",
                            supporting_files = support_dir,
                            directory = path, change_wd = FALSE,
                            create_on_github = FALSE,
                            git.user.name = "Test Name",
                            git.user.email = "test@email")
  support_new <- file.path(path, fs::path_file(support_dir), fs::path_file(support))
  expect_true(all(fs::file_exists(support_new)))
  committed_files <- workflowr:::get_committed_files(git2r::repository(path))
  expect_true(all(support_new %in% committed_files))
})

test_that("wflow_quickstart preserves relative paths to supporting files", {
  # This works because of knit_root_dir, which causes Rmd files to be
  # executed in the root of the project

  skip_on_cran()

  path <- workflowr:::absolute(fs::file_temp())
  on.exit(fs::dir_delete(path))
  rmd <- "files/test-wflow_quickstart/quickstart.Rmd"
  support_dir <- "files/test-wflow_quickstart/dir/"

  withr::local_options(list(workflowr.view = FALSE))
  quick <- wflow_quickstart(files = rmd, username = "username",
                            supporting_files = support_dir,
                            directory = path, change_wd = FALSE,
                            create_on_github = FALSE,
                            git.user.name = "Test Name",
                            git.user.email = "test@email")
  rmd_new <- file.path(path, "analysis", fs::path_file(rmd))
  expect_true(fs::file_exists(rmd_new))
  html_new <- workflowr:::to_html(rmd_new, outdir = file.path(path, "docs"))
  expect_true(fs::file_exists(html_new))
})

test_that("wflow_quickstart can delete or keep directory on error", {

  skip_on_cran()

  cwd <- getwd()
  on.exit(setwd(cwd), add = TRUE)
  path <- workflowr:::absolute(fs::file_temp())
  on.exit(fs::dir_delete(path), add = TRUE)

  rmd <- fs::file_temp(ext = ".Rmd")
  writeLines(c("```{r}",
               "stop('there was an error!!')",
               "```"),
             con = rmd)
  on.exit(fs::file_delete(rmd), add = TRUE)

  withr::local_options(list(workflowr.view = FALSE))

  # delete_on_error=TRUE
  expect_error(
    utils::capture.output(
      wflow_quickstart(files = rmd, username = "username",
                       directory = path, change_wd = TRUE,
                       create_on_github = FALSE,
                       git.user.name = "Test Name",
                       git.user.email = "test@email")
    ),
    "there was an error!!"
  )
  # Working directory should be restored
  expect_identical(getwd(), cwd)
  # New project directory should be deleted
  expect_false(fs::dir_exists(path))

  # delete_on_error=FALSE
  expect_error(
    utils::capture.output(
      wflow_quickstart(files = rmd, username = "username",
                       directory = path, change_wd = TRUE,
                       delete_on_error = FALSE,
                       create_on_github = FALSE,
                       git.user.name = "Test Name",
                       git.user.email = "test@email")
    ),
    "there was an error!!"
  )
  # Working directory should be the new project directory
  expect_identical(getwd(), path)
  # New project directory should not be deleted
  expect_true(fs::dir_exists(path))
  # There should be a Git repository
  expect_true(git2r::in_repository())
})

test_that("wflow_quickstart only accepts Rmd files", {

  rmd <- fs::file_temp(ext = ".Rmd")
  fs::file_create(rmd)
  on.exit(fs::file_delete(rmd), add = TRUE)

  nonrmd <- fs::file_temp(ext = ".md")
  fs::file_create(nonrmd)
  on.exit(fs::file_delete(nonrmd), add = TRUE)

  expect_error(
    wflow_quickstart(c(rmd, nonrmd)),
    "Only files with extension Rmd or rmd"
  )

  directory <- fs::file_temp()
  fs::dir_create(directory)
  on.exit(fs::dir_delete(directory), add = TRUE)

  expect_error(
    wflow_quickstart(c(rmd, directory)),
    "files cannot include a path to a directory"
  )

})

test_that("wflow_quickstart only deletes directory on error if it exists", {

  # Create an existing Git repo
  existing <- workflowr:::absolute(fs::file_temp())
  fs::dir_create(existing)
  on.exit(fs::dir_delete(existing))
  git2r::init(existing)

  # Plan to start the workflowr project within the existing Git repo
  path <- file.path(existing, "test")
  rmd <- fs::file_temp(ext = ".Rmd")
  fs::file_create(rmd)
  on.exit(fs::file_delete(rmd), add = TRUE)

  # wflow_quickstart should only return the wflow_start error
  expect_error(
    wflow_quickstart(files = rmd, username = "username",
                     directory = path, change_wd = FALSE,
                     create_on_github = FALSE,
                     git.user.name = "Test Name",
                     git.user.email = "test@email"),
    existing
  )
  expect_false(fs::dir_exists(path))
})

test_that("wflow_quickstart does not accept organization for hosting on GitLab", {

  rmd <- fs::file_temp(ext = ".Rmd")
  fs::file_create(rmd)
  on.exit(fs::file_delete(rmd), add = TRUE)

  expect_error(
    wflow_quickstart(rmd, organization = "gitlab-group", host = "gitlab"),
    "Do not use the argument"
  )
})

test_that("wflow_quickstart fails early if both username and organization are set", {

  rmd <- fs::file_temp(ext = ".Rmd")
  fs::file_create(rmd)
  on.exit(fs::file_delete(rmd), add = TRUE)
  directory <- "test"

  expect_error(
    wflow_quickstart(rmd, username = "personal-account",
                     organization = "github-org", directory = directory),
    "Cannot set both username and organization."
  )

  # Confirm that directory wasn't created
  expect_false(fs::dir_exists(directory))
})
