context("wflow_remotes")

# Setup ------------------------------------------------------------------------

# Setup workflowr project for testing
site_dir <- tempfile("test-wflow_remotes-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE))
# Delete workflowr project on exit
on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))
site_dir <- workflowr:::absolute(site_dir)

# Test create_remote_url -------------------------------------------------------

test_that("create_remote_url can create https URLs.", {
  expected <- "https://github.com/fakename/fakerepo.git"
  actual <- workflowr:::create_remote_url("fakename", "fakerepo", "https")
  expect_identical(actual, expected)
})

test_that("create_remote_url can create ssh URLs.", {
  expected <- "git@github.com:fakename/fakerepo.git"
  actual <- workflowr:::create_remote_url("fakename", "fakerepo", "ssh")
  expect_identical(actual, expected)
})

test_that("create_remote_url throws error for improper protocol.", {
  expect_error(workflowr:::create_remote_url("fakename", "fakerepo", "wrong"),
               "Invalid input for protocol. You entered: wrong")
})

# Test wflow_remotes -----------------------------------------------------------

test_that("wflow_remotes reports no remotes when none have been set.", {
  expect_message(remotes <- wflow_remotes(project = site_dir),
                 "The repository has no remotes set.")
  expect_true(length(remotes) == 0)
  expect_silent(wflow_remotes(verbose = FALSE, project = site_dir))
})

test_that("wflow_remotes can add a remote.", {
  expect_message(remotes <- wflow_remotes(remote = "origin", user = "fakename",
                                          repo = "fakerepo", project = site_dir),
                 "The repository has the following remotes set:")
  expect_true(remotes["origin"] == "https://github.com/fakename/fakerepo.git")
})

test_that("wflow_remotes can add a second remote.", {
  expect_message(remotes <- wflow_remotes(remote = "upstream", user = "fake2",
                                          repo = "fakerepo2", project = site_dir),
                 "The repository has the following remotes set:")
  expect_true(remotes["upstream"] == "https://github.com/fake2/fakerepo2.git")
  expect_true(length(remotes) == 2)
})

test_that("wflow_remotes can remove a remote.", {
  expect_message(remotes <- wflow_remotes(remote = "upstream",
                                          action = "remove", project = site_dir),
                 "The repository has the following remotes set:")
  expect_true(is.na(remotes["upstream"]))
})

test_that("wflow_remotes can change a URL from https to ssh.", {
  expect_message(remotes <- wflow_remotes(remote = "origin", user = "fakename",
                                          repo = "fakerepo", protocol = "ssh",
                                          action = "set_url", project = site_dir),
                 "The repository has the following remotes set:")
  expect_true(remotes["origin"] == "git@github.com:fakename/fakerepo.git")
})


test_that("wflow_remotes can remove the only remaining remote.", {
  expect_message(remotes <- wflow_remotes(remote = "origin",
                                          action = "remove", project = site_dir),
                 "The repository has no remotes set.")
  expect_true(length(remotes) == 0)
})

# Test error handling ----------------------------------------------------------

test_that("wflow_remotes rejects remotes with spaces or punctuation", {
  expect_error(wflow_remotes(remote = "a b", project = site_dir),
               "Limit the remote name to alphanumeric characters")
  expect_error(wflow_remotes(remote = "a\tb", project = site_dir),
               "Limit the remote name to alphanumeric characters")
  expect_error(wflow_remotes(remote = "a:b", project = site_dir),
               "Limit the remote name to alphanumeric characters")
  expect_error(wflow_remotes(remote = "a*b", project = site_dir),
               "Limit the remote name to alphanumeric characters")
  expect_error(wflow_remotes(remote = "a/b", project = site_dir),
               "Limit the remote name to alphanumeric characters")
})

test_that("wflow_remotes requires correct action", {
  expect_error(wflow_remotes(remote = "ab", action = "wrong", project = site_dir),
               "action must be add, remove, or set_url. You entered: wrong")
})

wflow_remotes(remote = "origin", user = "fakename", repo = "fakerepo",
              verbose = FALSE, project = site_dir)

test_that("wflow_remotes will not overwrite existing remotes", {
  expect_error(wflow_remotes(remote = "origin", user = "fakaename2",
                             repo = "fakerepo2", project = site_dir),
               "origin is already defined as a remote.")
})

test_that("wflow_remotes will not remove non-existent remotes", {
  expect_error(wflow_remotes(remote = "wrong", action = "remove",
                             project = site_dir),
               "wrong is not defined as a remote.")
})

test_that("wflow_remotes will not update URL of non-existent remotes", {
  expect_error(wflow_remotes(remote = "wrong", user = "fakename",
                             repo = "fakerepo", action = "set_url",
                             project = site_dir),
               "wrong is not defined as a remote.")
})
