# Setup ------------------------------------------------------------------------

source("setup.R")

skip_on_cran_windows()

# Test wflow_use_gitlab --------------------------------------------------------

test_that("wflow_use_gitlab automates local GitLab configuration", {
  path <- test_setup()
  on.exit(test_teardown(path))
  username <- "testuser"
  repository <- "testrepo"

  x <- wflow_use_gitlab(username, repository, project = path)

  # Renames the website directory from docs/ to public/
  s <- wflow_status(project = path)
  expect_identical(basename(s$docs), "public")
  expect_false(fs::dir_exists(file.path(s$root, "docs")))
  # Edits the setting output_dir in the file _site.yml
  site_yml_fname <- file.path(s$analysis, "_site.yml")
  site_yml <- yaml::yaml.load_file(site_yml_fname)
  output_dir <- site_yml$output_dir
  expect_identical("../public", output_dir)
  # Adds a link to the GitLab repository in the navigation bar
  expect_identical(site_yml$navbar$right[[1]]$icon,
                   workflowr:::get_fa_brand_icon("gitlab"))
  expect_identical(site_yml$navbar$right[[1]]$href,
                   sprintf("https://gitlab.com/%s/%s", username, repository))
  # Creates the required file .gitlab-ci.yml
  gitlab_yml <- file.path(s$root, ".gitlab-ci.yml")
  expect_true(file.exists(gitlab_yml))
  # Configures the Git remote settings to use GitLab
  remotes <- wflow_git_remote(verbose = FALSE, project = path)
  expect_equal(
    remotes["origin"],
    sprintf("https://gitlab.com/%s/%s.git", username, repository),
    ignore_attr = TRUE
  )

  # Confirm files were committed
  r <- git2r::repository(path = path)
  recent_commit <- git2r::commits(r, n = 1)[[1]]
  recent_commit_files <- workflowr:::obtain_files_in_commit(r, recent_commit)
  expect_true(workflowr:::absolute(site_yml_fname) %in% recent_commit_files)
  expect_true(workflowr:::absolute(gitlab_yml) %in% recent_commit_files)
})

test_that("wflow_use_gitlab throws error if username not set", {
  path <- test_setup()
  on.exit(test_teardown(path))
  username <- "testuser"
  repository <- "testrepo"

  expect_error(
    wflow_use_gitlab(project = path),
    "Unable to guess username. Please specify this argument."
  )

  expect_error(
    wflow_use_gitlab(username = NULL, repository = repository, project = path),
    "Unable to guess username. Please specify this argument."
  )
})

test_that("wflow_use_gitlab sets correct username", {
  path <- test_setup()
  on.exit(test_teardown(path))

  # Use value for remote named "origin"
  wflow_git_remote(remote = "origin", user = "testuser", repo = "testrepo",
                   project = path)
  x1 <- wflow_use_gitlab(project = path)
  expect_identical(x1$username, "testuser")
  origin <- wflow_git_remote(project = path)["origin"]
  expect_equal(
    origin,
    sprintf("https://gitlab.com/%s/%s.git", x1$username, x1$repository),
    ignore_attr = TRUE
  )

  # Will override based on input argument
  x2 <- wflow_use_gitlab(username = "argument", project = path)
  expect_identical(x2$username, "argument")
  origin <- wflow_git_remote(project = path)["origin"]
  expect_equal(
    origin,
    sprintf("https://gitlab.com/%s/%s.git", x2$username, x2$repository),
    ignore_attr = TRUE
  )
})

test_that("wflow_use_gitlab sets correct repository", {
  path <- test_setup()
  on.exit(test_teardown(path))
  username <- "testuser"

  # Default is to use name or root workflowr directory
  x1 <- wflow_use_gitlab(username = username, project = path)
  expect_identical(x1$repository, fs::path_file(path))
  origin <- wflow_git_remote(project = path)["origin"]
  expect_equal(
    origin,
    sprintf("https://gitlab.com/%s/%s.git", username, x1$repository),
    ignore_attr = TRUE
  )

  # Will override with value for remote named "origin"
  wflow_git_remote(remote = "origin", user = username,
                   repo = "not-the-same-as-project-root-dir",
                   action = "set_url", project = path)
  x2 <- wflow_use_gitlab(username = username, project = path)
  expect_identical(x2$repository, "not-the-same-as-project-root-dir")
  origin <- wflow_git_remote(project = path)["origin"]
  expect_equal(
    origin,
    sprintf("https://gitlab.com/%s/%s.git", username, x2$repository),
    ignore_attr = TRUE
  )

  # Will override based on input argument
  x3 <- wflow_use_gitlab(username = username, repository = "testrepo",
                         project = path)
  expect_identical(x3$repository, "testrepo")
  origin <- wflow_git_remote(project = path)["origin"]
  expect_equal(
    origin,
    sprintf("https://gitlab.com/%s/%s.git", username, x3$repository),
    ignore_attr = TRUE
  )
})

test_that("wflow_use_gitlab can disable navbar_link", {
  path <- test_setup()
  on.exit(test_teardown(path))
  username <- "testuser"
  repository <- "testrepo"

  x <- wflow_use_gitlab(username, repository, navbar_link = FALSE, project = path)

  s <- wflow_status(project = path)
  site_yml_fname <- file.path(s$analysis, "_site.yml")
  site_yml <- yaml::yaml.load_file(site_yml_fname)
  expect_null(site_yml$navbar$right[[1]]$icon)
  expect_null(site_yml$navbar$right[[1]]$href)
})

test_that("wflow_use_gitlab works when site has been published", {

  skip_on_cran()

  path <- test_setup()
  on.exit(test_teardown(path))
  username <- "testuser"
  repository <- "testrepo"

  s <- wflow_status(project = path)
  published <- wflow_publish(file.path(s$analysis, "*Rmd"), view = FALSE,
                             project = path)

  x <- wflow_use_gitlab(username, repository, project = path)

  # Renames the website directory from docs/ to public/
  s <- wflow_status(project = path)
  expect_identical(basename(s$docs), "public")
  expect_false(fs::dir_exists(file.path(s$root, "docs")))
  # Edits the setting output_dir in the file _site.yml
  site_yml_fname <- file.path(s$analysis, "_site.yml")
  site_yml <- yaml::yaml.load_file(site_yml_fname)
  output_dir <- site_yml$output_dir
  expect_identical("../public", output_dir)
  # Adds a link to the GitLab repository in the navigation bar
  expect_identical(site_yml$navbar$right[[1]]$icon,
                   workflowr:::get_fa_brand_icon("gitlab"))
  expect_identical(site_yml$navbar$right[[1]]$href,
                   sprintf("https://gitlab.com/%s/%s", username, repository))
  # Creates the required file .gitlab-ci.yml
  gitlab_yml <- file.path(s$root, ".gitlab-ci.yml")
  expect_true(file.exists(gitlab_yml))
  # Configures the Git remote settings to use GitLab
  remotes <- wflow_git_remote(verbose = FALSE, project = path)
  expect_equal(
    remotes["origin"],
    sprintf("https://gitlab.com/%s/%s.git", username, repository),
    ignore_attr = TRUE
  )

  # Confirm files were committed
  r <- git2r::repository(path = path)
  recent_commit <- git2r::commits(r, n = 1)[[1]]
  recent_commit_files <- workflowr:::obtain_files_in_commit(r, recent_commit)
  expect_true(workflowr:::absolute(site_yml_fname) %in% recent_commit_files)
  expect_true(workflowr:::absolute(gitlab_yml) %in% recent_commit_files)
  # Confirm website files were committed
  expect_true(all(basename(published$step3$commit_files) %in%
                  basename(recent_commit_files)))
})

test_that("wflow_use_gitlab can be run twice", {
  path <- test_setup()
  on.exit(test_teardown(path))
  username <- "testuser"
  repository <- "testrepo"

  x1 <- wflow_use_gitlab(username, repository, project = path)
  x2 <- wflow_use_gitlab(username, repository, project = path)

  expect_identical(x2$renamed, NA)
  expect_null(x2$files_git)
  expect_identical(x2$commit, NA)
  expect_identical(x2$config_remote, NA)
})

test_that("wflow_use_gitlab can be run after wflow_git_remote", {
  path <- test_setup()
  on.exit(test_teardown(path))
  username <- "testuser"
  repository <- "testrepo"

  remotes1 <- wflow_git_remote(remote = "origin", user = username,
                              repo = repository, protocol = "https",
                              action = "add", domain = "gitlab.com",
                              verbose = FALSE, project = path)
  # Purposefully have it guess username and repository from remote "origin"
  x <- wflow_use_gitlab(project = path)

  expect_identical(x$config_remote, NA)
  remotes2 <- wflow_git_remote(verbose = FALSE, project = path)
  expect_identical(remotes2["origin"], remotes1["origin"])
})

test_that("wflow_use_gitlab can be run after using GitHub remote", {
  path <- test_setup()
  on.exit(test_teardown(path))
  username <- "testuser"
  repository <- "testrepo"

  remotes1 <- wflow_git_remote(remote = "origin", user = username,
                               repo = repository, protocol = "https",
                               action = "add", domain = "gitlab.com",
                               verbose = FALSE, project = path)
  x <- wflow_use_gitlab(username, repository, project = path)

  remotes2 <- wflow_git_remote(verbose = FALSE, project = path)
  expect_equal(
    remotes2["origin"],
    sprintf("https://gitlab.com/%s/%s.git", username, repository),
    ignore_attr = TRUE
  )
})

test_that("wflow_use_gitlab works with ssh protocol", {
  path <- test_setup()
  on.exit(test_teardown(path))
  username <- "testuser"
  repository <- "testrepo"

  x <- wflow_use_gitlab(username, repository, protocol = "ssh", project = path)

  remotes <- wflow_git_remote(verbose = FALSE, project = path)
  expect_equal(
    remotes["origin"],
    sprintf("git@gitlab.com:%s/%s.git", username, repository),
    ignore_attr = TRUE
  )
})

test_that("wflow_use_gitlab works with different domain", {
  path <- test_setup()
  on.exit(test_teardown(path))
  username <- "testuser"
  repository <- "testrepo"

  x <- wflow_use_gitlab(username, repository, domain = "git.rcc.uchicago.edu",
                        project = path)

  remotes <- wflow_git_remote(verbose = FALSE, project = path)
  expect_equal(
    remotes["origin"],
    sprintf("https://git.rcc.uchicago.edu/%s/%s.git", username, repository),
    ignore_attr = TRUE
  )
})
