context("utility")

# Test to_html -----------------------------------------------------------------

test_that("to_html converts file extension Rmd", {
  expected <- "file.html"
  actual <- workflowr:::to_html("file.Rmd")
  expect_identical(actual, expected)
})

test_that("to_html converts file extension rmd", {
  expected <- "file.html"
  actual <- workflowr:::to_html("file.rmd")
  expect_identical(actual, expected)
})

test_that("to_html converts file extension even if it also appears in filename", {
  expected <- "Rmd.html"
  actual <- workflowr:::to_html("Rmd.Rmd")
  expect_identical(actual, expected)
})

test_that("to_html converts simple absolute path", {
  docs <- "/home/user/project/docs"
  expected <- file.path(docs, "file.html")
  actual <- workflowr:::to_html("/home/user/project/analysis/file.Rmd",
                                outdir = docs)
  expect_identical(actual, expected)
})

test_that("to_html converts simple relative path", {
  docs <- "docs"
  expected <- file.path(docs, "file.html")
  actual <- workflowr:::to_html("analysis/file.Rmd", outdir = docs)
  expect_identical(actual, expected)
})

test_that("to_html does not prepend ./", {
  docs <- "."
  expected <- "file.html"
  actual <- workflowr:::to_html("analysis/file.Rmd", outdir = docs)
  expect_identical(actual, expected)
})

test_that("to_html is vectorized", {
  docs <- "/home/user/project/docs"
  expected <- file.path(docs, c("1.html", "2.html", "3.html"))
  actual <- workflowr:::to_html(c("1.Rmd", "2.Rmd", "3.Rmd"),
                    outdir = docs)
  expect_identical(actual, expected)
})

test_that("to_html handles trailing slashes", {
  docs <- "/home/user/project/docs/"
  expected <- "/home/user/project/docs/file.html"
  actual <- workflowr:::to_html("/home/user/project/analysis/file.Rmd",
                                outdir = docs)
  expect_identical(actual, expected)
})

test_that("to_html throws errors for invalid extensions", {
  expect_error(workflowr:::to_html("file.md"), "Invalid file extension")
  expect_error(workflowr:::to_html("file.z"), "Invalid file extension")
  expect_error(workflowr:::to_html("file"), "Invalid file extension")
})

# Test absolute ----------------------------------------------------------------

test_that("absolute expands existing file", {
  path_rel <- "test-utility.R"
  path_abs <- workflowr:::absolute(path_rel)
  expect_true(fs::is_absolute_path(path_abs))
})

test_that("absolute expands existing directory", {
  path_rel <- "."
  path_abs <- workflowr:::absolute(path_rel)
  expect_true(fs::is_absolute_path(path_abs))
})

test_that("absolute expands non-existent file", {
  path_rel <- "non-existent-file"
  path_abs <- workflowr:::absolute(path_rel)
  expect_true(fs::is_absolute_path(path_abs))
})

test_that("absolute expands non-existent directory", {
  path_rel <- "a/b/c"
  path_abs <- workflowr:::absolute(path_rel)
  expect_true(fs::is_absolute_path(path_abs))
})

test_that("absolute removes duplicated forward slashes", {
  path_rel <- "a//b/c"
  path_abs <- workflowr:::absolute(path_rel)
  expect_false(stringr::str_detect(path_abs, "//"))
})

test_that("absolute removes duplicated back slashes", {
  path_rel <- "a\\\\b/c"
  path_abs <- workflowr:::absolute(path_rel)
  expect_false(stringr::str_detect(path_abs, "\\\\"))
})

test_that("absolute removes trailing forward slash(es)", {
  path_rel <- c("a/b/c/", "a/b/c//")
  path_abs <- workflowr:::absolute(path_rel)
  expect_false(all(stringr::str_detect(path_abs, "/$")))
})

test_that("absolute removes trailing back slash(es)", {
  path_rel <- c("a\\b\\c\\", "a\\b\\c\\\\")
  path_abs <- workflowr:::absolute(path_rel)
  expect_false(all(stringr::str_detect(path_abs, "\\$")))
})

test_that("absolute converts back slashes to forward slashes", {
  path_rel <- c("a\\b\\c\\", "a\\\\b\\\\c\\\\")
  path_abs <- workflowr:::absolute(path_rel)
  expect_false(all(stringr::str_detect(path_abs, "\\\\")))
})

test_that("absolute does not add any attributes to the character vector", {
  path_rel <- c("a/b/c", "x/y/z")
  path_abs <- workflowr:::absolute(path_rel)
  expect_true(is.character(path_abs))
  expect_true(is.null(attributes(path_abs)))
})

test_that("absolute returns NULL for NULL", {
  expect_identical(workflowr:::absolute(NULL), NULL)
})

test_that("absolute returns NA for NA", {
  expect_identical(workflowr:::absolute(NA), NA)
})

# Test relative ----------------------------------------------------------------

test_that("relative returns subdirectory", {
  path = "/test/location/project"
  start = "/test/location"
  expected <- "project"
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative returns nested subdirectories", {
  path = "/test/location/project/sub1/sub2"
  start = "/test/location"
  expected <- "project/sub1/sub2"
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative returns upstream directory", {
  path = "/test"
  start = "/test/location"
  expected <- ".."
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative returns multiple upstream directories", {
  path = "/test"
  start = "/test/location/project"
  expected <- "../.."
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative returns . when directories are the same", {
  path = "/test/location/project"
  start = "/test/location/project"
  expected <- "."
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative returns files in upstream directories", {
  path = "/test/location/project/sub1/file"
  start = "/test/location/project/sub2"
  expected <- "../sub1/file"
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative can handle tilde for home directory", {
  path = "~/test/location/project/sub1/file"
  start = "~/test/location/project/sub2"
  expected <- "../sub1/file"
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative can handle a tilde in an absolute path", {
  path = "/test/location/project/sub1/file~"
  start = "/test/location/project/sub2"
  expected <- "../sub1/file~"
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative returns NULL for NULL", {
  expect_identical(workflowr:::relative(NULL), NULL)
})

test_that("relative returns NA for NA", {
  expect_identical(workflowr:::relative(NA), NA)
})

test_that("relative works on vector input", {
  path = c("/test", "/test/location/subdir")
  start = "/test/location"
  expected <- c("..", "subdir")
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative works on relative paths to existing files", {
  fs::dir_create("x/y/z")
  on.exit(unlink("x", recursive = TRUE, force = TRUE))
  path = c("x", "x/y/z")
  start = "./x/y"
  expected <- c("..", "z")
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative handles NA and NULL", {
  expect_null(workflowr:::relative(NULL))
  expect_identical(NA, workflowr:::relative(NA))
  path = c("/test", NA, NULL, "/test/location/subdir")
  start = "/test/location"
  expected <- c("..", NA, NULL, "subdir")
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

# Test resolve_symlink ---------------------------------------------------------

test_that("absolute can resolve symlinks", {
  link <- workflowr:::absolute(tempfile())
  target <- workflowr:::absolute(".")
  on.exit(fs::link_delete(link))
  fs::link_create(target, link)
  expect_equal(workflowr:::absolute(link), target)
  # Non-existent path
  nonexist <- file.path(link, "x/y/z")
  expect_equal(workflowr:::absolute(nonexist), file.path(target, "x/y/z"))
})

test_that("relative can resolve symlinks", {
  link <- workflowr:::absolute(tempfile())
  target <- workflowr:::absolute(".")
  on.exit(fs::link_delete(link))
  fs::link_create(target, link)
  expect_equal(workflowr:::relative(link), ".")
  # Non-existent path
  nonexist <- file.path(link, "x/y/z")
  expect_equal(workflowr:::relative(nonexist), "x/y/z")
})

# Test get_host_from_remote --------------------------------------------------

tmp_dir <- tempfile("test-get_host_from_remote")
fs::dir_create(tmp_dir)
tmp_dir <- workflowr:::absolute(tmp_dir)

test_that("get_host_from_remote returns NA when no Git repo", {
  observed <- workflowr:::get_host_from_remote(tmp_dir)
  expect_identical(observed, NA_character_)
})

git2r::init(tmp_dir)
r <- git2r::repository(tmp_dir)

test_that("get_host_from_remote returns NA when no remotes", {
  observed <- workflowr:::get_host_from_remote(tmp_dir)
  expect_identical(observed, NA_character_)
})

wflow_git_remote(remote = "nonstandard", user = "testuser", repo = "testrepo",
                 verbose = FALSE, project = tmp_dir)

test_that("get_host_from_remote returns NA when no origin", {
  observed <- workflowr:::get_host_from_remote(tmp_dir)
  expect_identical(observed, NA_character_)
})

wflow_git_remote(remote = "origin", user = "testuser2", repo = "testrepo",
                 verbose = FALSE, project = tmp_dir)

test_that("get_host_from_remote works with HTTPS protocol", {
  observed <- workflowr:::get_host_from_remote(tmp_dir)
  expect_identical(observed, "https://github.com/testuser2/testrepo")
})

wflow_git_remote(remote = "origin", user = "testuser2", repo = "testrepo",
                 protocol = "ssh", action = "set_url", verbose = FALSE,
                 project = tmp_dir)

test_that("get_host_from_remote works with SSH protocol", {
  observed <- workflowr:::get_host_from_remote(tmp_dir)
  expect_identical(observed, "https://github.com/testuser2/testrepo")
})

wflow_git_remote(remote = "origin", action = "remove", verbose = FALSE,
                 project = tmp_dir)

test_that("get_host_from_remote works with GitLab.com HTTPS protocol", {
  wflow_git_remote(remote = "origin", user = "testuser2", repo = "testrepo",
                   domain = "gitlab.com", project = tmp_dir)
  observed <- workflowr:::get_host_from_remote(tmp_dir)
  expect_identical(observed, "https://gitlab.com/testuser2/testrepo")
  wflow_git_remote(remote = "origin", action = "remove", project = tmp_dir)
})

test_that("get_host_from_remote works with GitLab.com SSH protocol", {
  wflow_git_remote(remote = "origin", user = "testuser2", repo = "testrepo",
                   protocol = "ssh", domain = "gitlab.com", project = tmp_dir)
  observed <- workflowr:::get_host_from_remote(tmp_dir)
  expect_identical(observed, "https://gitlab.com/testuser2/testrepo")
  wflow_git_remote(remote = "origin", action = "remove", project = tmp_dir)
})

test_that("get_host_from_remote works with custom HTTPS protocol", {
  wflow_git_remote(remote = "origin", user = "testuser2", repo = "testrepo",
                   domain = "git.rcc.uchicago.edu", project = tmp_dir)
  observed <- workflowr:::get_host_from_remote(tmp_dir)
  expect_identical(observed, "https://git.rcc.uchicago.edu/testuser2/testrepo")
  wflow_git_remote(remote = "origin", action = "remove", project = tmp_dir)
})

test_that("get_host_from_remote works with custom SSH protocol", {
  wflow_git_remote(remote = "origin", user = "testuser2", repo = "testrepo",
                   protocol = "ssh", domain = "git.rcc.uchicago.edu", project = tmp_dir)
  observed <- workflowr:::get_host_from_remote(tmp_dir)
  expect_identical(observed, "https://git.rcc.uchicago.edu/testuser2/testrepo")
  wflow_git_remote(remote = "origin", action = "remove", project = tmp_dir)
})

unlink(tmp_dir, recursive = TRUE)
rm(r, tmp_dir)
