context("vig-gitlab")

# Testing similar workflow as in vignette wflow-06-gitlab.Rmd

# Setup ------------------------------------------------------------------------

site_dir <- tempfile("new-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE, user.name = "Test Name",
                             user.email = "test@email"))
on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))
site_dir <- workflowr:::absolute(site_dir)
r <- git2r::repository(path = site_dir)
s <- wflow_status(project = site_dir)

# Test GitLab instructions -----------------------------------------------------

test_that("Add GitLab remote", {
  remote <- wflow_git_remote(remote = "origin", user = "testname", repo = "testrepo",
                             domain = "gitlab.com", verbose = FALSE,
                             project = site_dir)
  expect_equivalent(remote["origin"], "https://gitlab.com/testname/testrepo.git")
})

test_that("Setup GitLab infrastructure", {

  skip_on_cran()

  wflow_publish(rownames(s$status), view = FALSE, project = site_dir)
  gitlab <- wflow_use_gitlab(project = site_dir)
  s <- wflow_status(project = site_dir)
  expect_identical(absolute(s$docs), file.path(site_dir, "public"))
  expect_true(fs::dir_exists(s$docs))
  expect_false(fs::dir_exists(file.path(site_dir, "docs")))
  expect_true(fs::file_exists(file.path(site_dir, ".gitlab-ci.yml")))
  git_status <- git2r::status(r)
  expect_equal(length(git_status$staged), 0)
  expect_equal(length(git_status$unstaged), 0)
  expect_equal(length(git_status$untracked), 0)
  site_yml_fname <- file.path(s$analysis, "_site.yml")
  site_yml <- yaml::yaml.load_file(site_yml_fname)
  expect_identical(site_yml$navbar$right[[1]][["icon"]], "fa-gitlab")
  expect_identical(site_yml$output_dir, "../public")
})

test_that("Republish analyses", {

  skip_on_cran()

  pub <- wflow_publish(republish = TRUE, view = FALSE, project = site_dir)
  expect_false(is.null(pub$step2$html))
  expect_identical(unique(basename(dirname(pub$step2$html))), "public")
  html_committed <- stringr::str_subset(pub$step3$commit_files, "html$")
  expect_identical(unique(basename(dirname(html_committed))), "public")
  git_status <- git2r::status(r)
  expect_equal(length(git_status$staged), 0)
  expect_equal(length(git_status$unstaged), 0)
  expect_equal(length(git_status$untracked), 0)
})
