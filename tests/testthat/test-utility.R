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
  path <- "/test/location/project"
  start <- "/test/location"
  expected <- "project"
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative returns nested subdirectories", {
  path <- "/test/location/project/sub1/sub2"
  start <- "/test/location"
  expected <- "project/sub1/sub2"
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative returns upstream directory", {
  path <- "/test"
  start <- "/test/location"
  expected <- ".."
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative returns multiple upstream directories", {
  path <- "/test"
  start <- "/test/location/project"
  expected <- "../.."
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative returns . when directories are the same", {
  path <- "/test/location/project"
  start <- "/test/location/project"
  expected <- "."
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative returns files in upstream directories", {
  path <- "/test/location/project/sub1/file"
  start <- "/test/location/project/sub2"
  expected <- "../sub1/file"
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative can handle tilde for home directory", {
  path <- "~/test/location/project/sub1/file"
  start <- "~/test/location/project/sub2"
  expected <- "../sub1/file"
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative can handle a tilde in an absolute path", {
  path <- "/test/location/project/sub1/file~"
  start <- "/test/location/project/sub2"
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
  path <- c("/test", "/test/location/subdir")
  start <- "/test/location"
  expected <- c("..", "subdir")
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative works on relative paths to existing files", {
  fs::dir_create("x/y/z")
  on.exit(unlink("x", recursive = TRUE, force = TRUE))
  path <- c("x", "x/y/z")
  start <- "./x/y"
  expected <- c("..", "z")
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative handles NA and NULL", {
  expect_null(workflowr:::relative(NULL))
  expect_identical(NA, workflowr:::relative(NA))
  path <- c("/test", NA, NULL, "/test/location/subdir")
  start <- "/test/location"
  expected <- c("..", NA, NULL, "subdir")
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative returns absolute path when path and start on different Windows drive", {

  if (.Platform$OS.type != "windows") skip("Only relevant on Windows")

  path <- "D:/a/file"
  start <- "C:/Users/CRAN"
  expected <- path
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative returns relative path when path and start on same Windows drive", {

  if (.Platform$OS.type != "windows") skip("Only relevant on Windows")

  path <- "C:/a/file"
  start <- "C:/Users/CRAN"
  expected <- "../../a/file"
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative throws error when paths have different Windows drive", {

  if (.Platform$OS.type != "windows") skip("Only relevant on Windows")

  path <- c("C:/a/file", "D:/a/file")
  start <- "C:/Users/CRAN"
  expect_error(workflowr:::relative(path, start),
               "All paths must be on the same Windows drive")
})

test_that("relative can handle Windows drives on winbuilder", {

  if (.Platform$OS.type != "windows") skip("Only relevant on Windows")

  path <- c("D:/a/file1", "D:/a/file2")
  start <- "c:/Users/CRAN"
  expected <- path
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)
})

test_that("relative handles NA and NULL with Windows drives", {

  if (.Platform$OS.type != "windows") skip("Only relevant on Windows")

  path <- c("C:/a/file1", NA, NULL, "C:/a/file2")
  start <- "C:/Users/CRAN"
  expected <- c("../../a/file1", NA, "../../a/file2")
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)

  path <- c("D:/a/file1", NA, NULL, "D:/a/file2")
  start <- "C:/Users/CRAN"
  expected <- path
  actual <- workflowr:::relative(path, start)
  expect_identical(actual, expected)

  path <- c("C:/a/file", NA, NULL, "D:/a/file")
  start <- "C:/Users/CRAN"
  expect_error(workflowr:::relative(path, start),
               "All paths must be on the same Windows drive")

  path <- c("D:/a/file1", NA, NULL, "D:/a/file2")
  start <- "c:/Users/CRAN"
  expected <- path
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

# Test toupper_win_drive -------------------------------------------------------

test_that("toupper_win_drive capitalizes lowercase Windows drives", {
  expect_equal(workflowr:::toupper_win_drive("a:/a/b/c"), "A:/a/b/c")
  expect_equal(workflowr:::toupper_win_drive("b:/a/b/c"), "B:/a/b/c")
  expect_equal(workflowr:::toupper_win_drive("c:/a/b/c"), "C:/a/b/c")
  expect_equal(workflowr:::toupper_win_drive("d:/a/b/c"), "D:/a/b/c")
  expect_equal(workflowr:::toupper_win_drive("x:/a/b/c"), "X:/a/b/c")
  expect_equal(workflowr:::toupper_win_drive("y:/a/b/c"), "Y:/a/b/c")
  expect_equal(workflowr:::toupper_win_drive("z:/a/b/c"), "Z:/a/b/c")
})

test_that("toupper_win_drive is vectorized", {
  expect_equal(workflowr:::toupper_win_drive(c("c:/a/b/c", "d:/a/b/c")),
               c("C:/a/b/c", "D:/a/b/c"))
})

test_that("toupper_win_drive ignores Unix-like paths", {
  expect_equal(workflowr:::toupper_win_drive("/a/b/c"), "/a/b/c")
  expect_equal(workflowr:::toupper_win_drive("/tmp"), "/tmp")
  expect_equal(workflowr:::toupper_win_drive("/"), "/")
})

test_that("toupper_win_drive ignores relative paths", {
  expect_equal(workflowr:::toupper_win_drive(".."), "..")
  expect_equal(workflowr:::toupper_win_drive("./tmp"), "./tmp")
  expect_equal(workflowr:::toupper_win_drive("../../a/b/c"), "../../a/b/c")
})

test_that("toupper_win_drive has no effect if drive is already capitalized", {
  expect_equal(workflowr:::toupper_win_drive("A:/a/b/c"), "A:/a/b/c")
  expect_equal(workflowr:::toupper_win_drive("B:/a/b/c"), "B:/a/b/c")
  expect_equal(workflowr:::toupper_win_drive("C:/a/b/c"), "C:/a/b/c")
  expect_equal(workflowr:::toupper_win_drive("D:/a/b/c"), "D:/a/b/c")
  expect_equal(workflowr:::toupper_win_drive("X:/a/b/c"), "X:/a/b/c")
  expect_equal(workflowr:::toupper_win_drive("Y:/a/b/c"), "Y:/a/b/c")
  expect_equal(workflowr:::toupper_win_drive("Z:/a/b/c"), "Z:/a/b/c")
})

test_that("toupper_win_drive ignores any potential drive that is not a single letter", {
  expect_equal(workflowr:::toupper_win_drive("1:/"), "1:/")
  expect_equal(workflowr:::toupper_win_drive("abc:/"), "abc:/")
  expect_equal(workflowr:::toupper_win_drive("/a:/b/c"), "/a:/b/c")
})

# Test get_win_drive -----------------------------------------------------------

test_that("get_win_drive returns the Windows drive", {
  expect_equal(get_win_drive("C:/a/b/c"), "C:")
  expect_equal(get_win_drive("D:/a/b/c"), "D:")
  expect_equal(get_win_drive(c("C:/a/b/c", "D:/a/b/c")), c("C:", "D:"))
})

# Test get_host_from_remote ----------------------------------------------------

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

# Test status_to_df and df_to_status -------------------------------------------

test_that("status_to_df converts to data frame and df_to_status reverts", {
  files_staged <- structure(list(modified = "staged-1.txt"),
                            .Names = "modified")
  files_unstaged <- structure(list(modified = "unstaged-1.txt"),
                              .Names = "modified")
  files_untracked <- structure(list(untracked = "untracked-1.txt",
                                    untracked = "untracked-2.txt"),
                               .Names = c("untracked", "untracked"))
  input <- structure(list(staged = files_staged,
                          unstaged = files_unstaged,
                          untracked = files_untracked),
                          .Names = c("staged", "unstaged", "untracked"),
                          class = "git_status")

  expected <- data.frame(
    status = c("staged", "unstaged", "untracked", "untracked"),
    substatus = c("modified", "modified", "untracked", "untracked"),
    file = c("staged-1.txt", "unstaged-1.txt", "untracked-1.txt", "untracked-2.txt"),
    stringsAsFactors = FALSE)

  observed <- workflowr:::status_to_df(input)
  expect_identical(observed, expected)

  # Revert to git_status
  expect_identical(workflowr:::df_to_status(observed), input)
})

test_that("status_to_df and df_to_status can handle empty status", {
  input <- structure(list(staged = structure(list(), .Names = character(0)),
                          unstaged = structure(list(), .Names = character(0)),
                          untracked = structure(list(), .Names = character(0))),
                     .Names = c("staged", "unstaged", "untracked"),
                     class = "git_status")

  expected <- data.frame(status = character(), substatus = character(),
                         file =  character(), stringsAsFactors = FALSE)

  observed <- workflowr:::status_to_df(input)
  expect_identical(observed, expected)

  # Revert to git_status
  expect_identical(workflowr:::df_to_status(observed), input)
})

# Test file_is_executable ------------------------------------------------------

test_that("file_executable returns FALSE for non-executable file", {

  if (.Platform$OS.type == "windows")
    skip("File permissions are different on Windows")

  f <- fs::file_temp()
  fs::file_create(f)
  on.exit(fs::file_delete(f))

  expect_false(workflowr:::file_is_executable(f))
})

test_that("file_executable returns TRUE for executable file", {

  if (.Platform$OS.type == "windows")
    skip("File permissions are different on Windows")

  f <- fs::file_temp()
  fs::file_create(f)
  on.exit(fs::file_delete(f))
  fs::file_chmod(f, "a+x")

  expect_true(workflowr:::file_is_executable(f))
})

# Test wflow_dependson ---------------------------------------------------------

test_that("wflow_dependson returns labels of cached chunks", {

  skip_on_cran()

  tmp_dir <- tempfile()
  fs::dir_create(tmp_dir)
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # If no caching is used, dependson=NULL.
  rmd <- file.path(tmp_dir, "file.Rmd")
  fs::file_copy("files/test-wflow_html/dependson-cache-none.Rmd", rmd)
  html <- render(rmd, quiet = TRUE)
  expect_true(fs::file_exists(html))
  rds <- file.path(tmp_dir, "labels.rds")
  labels <- readRDS(rds)
  expect_null(labels)

  # If global cache=FALSE, but specific chunks have cache=TRUE, only depend on
  # these chunks.
  rmd <- file.path(tmp_dir, "file.Rmd")
  fs::file_copy("files/test-wflow_html/dependson-cache-local.Rmd", rmd,
                overwrite = TRUE)
  html <- render(rmd, quiet = TRUE)
  expect_true(fs::file_exists(html))
  rds <- file.path(tmp_dir, "labels.rds")
  labels <- readRDS(rds)
  expect_identical(labels, c("plot-one", "plot-two"))

  # If global cache=TRUE, depend on all chunks except those with cache=FALSE.
  rmd <- file.path(tmp_dir, "file.Rmd")
  fs::file_copy("files/test-wflow_html/dependson-cache-global.Rmd", rmd,
                overwrite = TRUE)
  html <- render(rmd, quiet = TRUE)
  expect_true(fs::file_exists(html))
  rds <- file.path(tmp_dir, "labels.rds")
  labels <- readRDS(rds)
  expect_identical(labels, c("setup", "plot-one", "plot-three",
                             "session-info-chunk-inserted-by-workflowr"))
})
