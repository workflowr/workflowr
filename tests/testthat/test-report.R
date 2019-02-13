context("report")

# Setup ------------------------------------------------------------------------

source("setup.R")

# Test get_versions and get_versions_fig ---------------------------------------

test_that("get_versions and get_versions_fig insert GitHub URL if available", {

  skip_on_cran()

  tmp_dir <- tempfile()
  tmp_start <- wflow_start(tmp_dir, change_wd = FALSE, user.name = "Test Name",
                             user.email = "test@email")
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  rmd <- file.path(tmp_dir, "analysis", "file.Rmd")
  lines <- c("---",
             "output: workflowr::wflow_html",
             "---",
             "",
             "```{r chunkname}",
             "plot(1:10)",
             "```")
  writeLines(lines, rmd)

  # Add remote
  tmp_remote <- wflow_git_remote("origin", "testuser", "testrepo",
                                 verbose = FALSE, project = tmp_dir)

  # Go through a few commit cycles
  for (i in 1:3) {
    cat("edit", file = rmd, append = TRUE)
    tmp_publish <- wflow_publish(rmd, view = FALSE, project = tmp_dir)
  }

  r <- git2r::repository(tmp_dir)
  blobs <- git2r::odb_blobs(r)
  output_dir <- workflowr:::get_output_dir(file.path(tmp_dir, "analysis/"))
  github <- workflowr:::get_host_from_remote(tmp_dir)
  versions <- workflowr:::get_versions(input = rmd, output_dir, blobs, r, github)
  expect_true(any(stringr::str_detect(versions, github)))
  fig <- file.path(output_dir, "figure", basename(rmd), "chunkname-1.png")
  versions_fig <- get_versions_fig(fig, r, github)
  expect_true(any(stringr::str_detect(versions_fig, github)))
})


# Test check_vc ----------------------------------------------------------------

tmp_dir <- tempfile("test-check_vc")
fs::dir_create(tmp_dir)
tmp_dir <- workflowr:::absolute(tmp_dir)
rmd <- file.path(tmp_dir, "file.Rmd")
writeLines(letters, rmd)

test_that("check_vc reports lack of Git repo", {
  observed <- workflowr:::check_vc(rmd, r = NULL, s = NULL, github = NA_character_)
  expect_false(observed$pass)
  expect_identical(observed$summary,
                   "<strong>Repository version:</strong> no version control")
})

git2r::init(tmp_dir)
r <- git2r::repository(tmp_dir)
git2r::config(r, user.name = "Test Name", user.email = "test@email")
s <- git2r::status(r, ignored = TRUE)

test_that("check_vc reports Git repo even if no commits", {
  observed <- workflowr:::check_vc(rmd, r = r, s = s, github = NA_character_)
  expect_true(observed$pass)
  expect_identical(observed$summary,
                   "<strong>Repository version:</strong> No commits yet")
})

workflowr:::git2r_add(r, rmd)
git2r::commit(r, "Add rmd")
s <- git2r::status(r, ignored = TRUE)
current_commit <- git2r_slot(git2r::commits(r)[[1]], "sha")
commit_to_display <- workflowr:::shorten_sha(current_commit)

test_that("check_vc reports Git repo", {
  observed <- workflowr:::check_vc(rmd, r = r, s = s, github = NA_character_)
  expect_true(observed$pass)
  expect_identical(observed$summary,
                   sprintf("<strong>Repository version:</strong> %s",
                           commit_to_display))
})

test_that("check_vc reports Git repo and can add GitHub URL", {
  github <- "https://github.com/jdblischak/workflowr"
  observed <- workflowr:::check_vc(rmd, r = r, s = s, github = github)
  expect_true(observed$pass)
  expect_identical(observed$summary,
                   sprintf("<strong>Repository version:</strong> <a href=\"%s/tree/%s\" target=\"_blank\">%s</a>",
                           github, current_commit, commit_to_display))
})

test_that("check_vc ignores *html, *png, and site_libs", {
  fname_html <- file.path(tmp_dir, "file.html")
  fs::file_create(fname_html)
  fname_png <- file.path(tmp_dir, "file.png")
  fs::file_create(fname_png)
  site_libs <- file.path(tmp_dir, "site_libs")
  fs::dir_create(site_libs)
  site_libs_readme <- file.path(site_libs, "README.md")
  fs::file_create(site_libs_readme)

  observed <- workflowr:::check_vc(rmd, r = r, s = s, github = NA_character_)

  expect_true(observed$pass)
  expect_false(grepl(basename(fname_html), observed$details))
  expect_false(grepl(basename(fname_png), observed$details))
  expect_false(grepl(basename(site_libs), observed$details))
  expect_true(grepl("working directory clean", observed$details))
})

rmd2 <- file.path(tmp_dir, "file2.Rmd")
fs::file_create(rmd2)
s <- git2r::status(r, ignored = TRUE)

test_that("check_vc reports uncommitted Rmd files", {
  observed <- workflowr:::check_vc(rmd, r = r, s = s, github = NA_character_)

  expect_true(observed$pass)
  expect_true(grepl(basename(rmd2), observed$details))
})

unlink(tmp_dir, recursive = TRUE)
rm(r, rmd, rmd2, s, tmp_dir, current_commit, commit_to_display)

# Test check_sessioninfo -------------------------------------------------------

rmd <- tempfile("check-sessioninfo-", fileext = ".Rmd")
fs::file_copy("files/example.Rmd", rmd)

test_that("check_sessioninfo reports sessioninfo reported as an option", {
  observed <- workflowr:::check_sessioninfo(rmd, "sessionInfo()")
  expect_true(observed$pass)
  expect_identical(observed$summary, "<strong>Session information:</strong> recorded")
})

test_that("check_sessioninfo reports missing sessioninfo", {
  observed <- workflowr:::check_sessioninfo(rmd, "")
  expect_false(observed$pass)
  expect_identical(observed$summary, "<strong>Session information:</strong> unavailable")
})

cat("\nsessionInfo()\n", file = rmd, append = TRUE)

test_that("check_sessioninfo reports sessioninfo in file as sessionInfo()", {
  observed <- workflowr:::check_sessioninfo(rmd, "")
  expect_true(observed$pass)
  expect_identical(observed$summary, "<strong>Session information:</strong> recorded")
})

lines <- readLines(rmd)
writeLines(stringr::str_replace(lines, "sessionInfo", "session_info"),
           con = rmd)

test_that("check_sessioninfo reports sessioninfo in file as session_info() from devtools", {
  observed <- workflowr:::check_sessioninfo(rmd, "")
  expect_true(observed$pass)
  expect_identical(observed$summary, "<strong>Session information:</strong> recorded")
})

test_that("check_sessioninfo reports sessioninfo when in file and an option", {
  observed <- workflowr:::check_sessioninfo(rmd, "sessionInfo()")
  expect_true(observed$pass)
  expect_identical(observed$summary, "<strong>Session information:</strong> recorded")
})

unlink(rmd)
rm(rmd, lines)

# Test check_seed --------------------------------------------------------------

test_that("check_seed reports valid seed", {
  seed <- 1
  observed <- workflowr:::check_seed(seed)
  expect_true(observed$pass)
  expect_identical(observed$summary,
                   sprintf("<strong>Seed:</strong> <code>set.seed(%d)</code>", seed))
})

test_that("check_seed reports invalid seed", {
  seed <- ""
  observed <- workflowr:::check_seed(seed)
  expect_false(observed$pass)
  expect_identical(observed$summary, "<strong>Seed:</strong> none")
})

# Test check_environment -------------------------------------------------------

test_that("check_environment reports empty environment", {
  envir_empty <- new.env()
  on.exit(rm(envir_empty))
  observed <- workflowr:::check_environment(envir_empty)
  expect_true(observed$pass)
  expect_identical(observed$summary, "<strong>Environment:</strong> empty")
})

test_that("check_environment reports non-empty environment", {
  envir_objects <- new.env()
  on.exit(rm(envir_objects))
  envir_objects$long_variable_name <- 1
  observed <- workflowr:::check_environment(envir_objects)
  expect_false(observed$pass)
  expect_identical(observed$summary, "<strong>Environment:</strong> objects present")
  expect_true(grepl("long_variable_name", observed$details))
})

# Test check_cache -------------------------------------------------------------

test_that("check_cache passes if no cached chunks present", {
  x <- fs::file_temp()
  fs::dir_create(x)
  on.exit(fs::dir_delete(x))

  rmd <- file.path(x, "test.Rmd")
  fs::file_create(rmd)

  observed <- workflowr:::check_cache(rmd)
  expect_true(observed$pass)
  expect_identical(observed$summary, "<strong>Cache:</strong> none")
})

test_that("check_cache passes if empty cache directory", {
  x <- fs::file_temp()
  fs::dir_create(x)
  on.exit(fs::dir_delete(x))

  rmd <- file.path(x, "test.Rmd")
  fs::file_create(rmd)

  rmd_cache <- file.path(x, "test_cache")
  fs::dir_create(rmd_cache)

  observed <- workflowr:::check_cache(rmd)
  expect_true(observed$pass)

  rmd_cache_html <- file.path(rmd_cache, "html")
  fs::dir_create(rmd_cache_html)

  observed <- workflowr:::check_cache(rmd)
  expect_true(observed$pass)
})

test_that("check_cache fails if cached chunks present", {
  x <- fs::file_temp()
  fs::dir_create(x)
  on.exit(fs::dir_delete(x))

  rmd <- file.path(x, "test.Rmd")
  fs::file_create(rmd)

  rmd_cache_html <- file.path(x, "test_cache/html")
  fs::dir_create(rmd_cache_html, recursive = TRUE)

  cached_chunk_1 <- file.path(rmd_cache_html, "chunk-name_29098n0b4h849.RData")
  fs::file_create(cached_chunk_1)
  cached_chunk_2 <- file.path(rmd_cache_html, "a_chunk_29098n0b4h849.RData")
  fs::file_create(cached_chunk_2)

  observed <- workflowr:::check_cache(rmd)
  expect_false(observed$pass)
  expect_identical(observed$summary, "<strong>Cache:</strong> detected")
  expect_true(stringr::str_detect(observed$details, "<li>chunk-name</li>"))
  expect_true(stringr::str_detect(observed$details, "<li>a_chunk</li>"))
  expect_true(stringr::str_detect(observed$details, "<code>test_cache</code>"))
})

# Test check_rmd ---------------------------------------------------------------

tmp_dir <- tempfile("test-check_rmd")
fs::dir_create(tmp_dir)
tmp_dir <- workflowr:::absolute(tmp_dir)
git2r::init(tmp_dir)
r <- git2r::repository(tmp_dir)
git2r::config(r, user.name = "Test Name", user.email = "test@email")
rmd <- file.path(tmp_dir, "file.Rmd")
writeLines(letters, rmd)
s <- git2r::status(r, ignored = TRUE)

test_that("check_rmd reports an untracked Rmd file", {
  observed <- workflowr:::check_rmd(rmd, r, s)
  expect_false(observed$pass)
  expect_identical(observed$summary,
                   "<strong>R Markdown file:</strong> uncommitted changes")
})

test_that("check_rmd reports an ignored Rmd file", {
  gitignore <- file.path(tmp_dir, ".gitignore")
  on.exit(unlink(gitignore))
  writeLines("*Rmd", gitignore)
  s <- git2r::status(r, ignored = TRUE)
  observed <- workflowr:::check_rmd(rmd, r, s)
  expect_false(observed$pass)
  expect_identical(observed$summary,
                   "<strong>R Markdown file:</strong> uncommitted changes")
  expect_true(grepl("ignored", observed$details))
})

workflowr:::git2r_add(r, rmd)
s <- git2r::status(r, ignored = TRUE)

test_that("check_rmd reports a staged Rmd file", {
  observed <- workflowr:::check_rmd(rmd, r, s)
  expect_false(observed$pass)
  expect_identical(observed$summary,
                   "<strong>R Markdown file:</strong> uncommitted changes")
  expect_true(grepl("staged changes", observed$details))
})

git2r::commit(r, "add rmd file")
s <- git2r::status(r, ignored = TRUE)

test_that("check_rmd reports a committed (up-to-date) Rmd file", {
  observed <- workflowr:::check_rmd(rmd, r, s)
  expect_true(observed$pass)
  expect_identical(observed$summary,
                   "<strong>R Markdown file:</strong> up-to-date")
  expect_true(grepl("committed", observed$details))
})

writeLines(LETTERS, rmd)
s <- git2r::status(r, ignored = TRUE)

test_that("check_rmd reports an unstaged Rmd file", {
  observed <- workflowr:::check_rmd(rmd, r, s)
  expect_false(observed$pass)
  expect_identical(observed$summary,
                   "<strong>R Markdown file:</strong> uncommitted changes")
  expect_true(grepl("unstaged changes", observed$details))
})

unlink(tmp_dir, recursive = TRUE)
rm(r, rmd, s, tmp_dir)

# Test create_report -----------------------------------------------------------

test_that("create_report reports knit directory", {

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  input <- file.path(path, "analysis/index.Rmd")
  output_dir <- file.path(path, "docs")
  has_code <- TRUE
  opts <- list(seed = 1, github = NA, sessioninfo = "sessionInfo()",
               fig_path_ext = FALSE)

  # In the project root
  opts$knit_root_dir <- path
  report <- create_report(input, output_dir, has_code, opts)
  expected <- glue::glue("<code>{fs::path_file(path)}/</code>")
  expect_true(stringr::str_detect(report, expected))

  # In analysis
  opts$knit_root_dir <- file.path(path, "analysis")
  report <- create_report(input, output_dir, has_code, opts)
  expected <- glue::glue("<code>{file.path(fs::path_file(path), \"analysis\")}/</code>")
  expect_true(stringr::str_detect(report, expected))

  # In code
  opts$knit_root_dir <- file.path(path, "code")
  report <- create_report(input, output_dir, has_code, opts)
  expected <- glue::glue("<code>{file.path(fs::path_file(path), \"code\")}/</code>")
  expect_true(stringr::str_detect(report, expected))

  # In home directory
  opts$knit_root_dir <- "~"
  report <- create_report(input, output_dir, has_code, opts)
  expected <- glue::glue("<code>~/</code>")
  expect_true(stringr::str_detect(report, expected))

  # In temp directory
  opts$knit_root_dir <- fs::file_temp()
  report <- create_report(input, output_dir, has_code, opts)
  expected <- glue::glue("<code>{opts$knit_root_dir}/</code>")
  expect_true(stringr::str_detect(report, expected))
})

# Test detect_code -------------------------------------------------------------

test_that("detect_code detects a code chunk", {
  lines <- c("```{r}",
             "x <- 1",
             "```")
  fname <- tempfile()
  on.exit(unlink(fname))
  writeLines(lines, fname)
  expect_true(detect_code(fname))
})

test_that("detect_code detects a named code chunk with options", {
  lines <- c("```{r name, fig.width = 3}",
             "x <- 1",
             "```")
  fname <- tempfile()
  on.exit(unlink(fname))
  writeLines(lines, fname)
  expect_true(detect_code(fname))
})

test_that("detect_code detects a named code chunk with options", {
  lines <- c("```{r name, fig.width = 3}",
             "x <- 1",
             "```")
  fname <- tempfile()
  on.exit(unlink(fname))
  writeLines(lines, fname)
  expect_true(detect_code(fname))
})

test_that("detect_code detects a python code chunk", {
  lines <- c("```{python}",
             "x = 1",
             "```")
  fname <- tempfile()
  on.exit(unlink(fname))
  writeLines(lines, fname)
  expect_true(detect_code(fname))
})

test_that("detect_code detects a bash code chunk", {
  lines <- c("```{bash}",
             "x=1",
             "```")
  fname <- tempfile()
  on.exit(unlink(fname))
  writeLines(lines, fname)
  expect_true(detect_code(fname))
})

test_that("detect_code detects inline code", {
  lines <- c("`r 1`")
  fname <- tempfile()
  on.exit(unlink(fname))
  writeLines(lines, fname)
  expect_true(detect_code(fname))
})

test_that("detect_code detects inline code with spaces", {
  lines <- c("`r 1 + 2`")
  fname <- tempfile()
  on.exit(unlink(fname))
  writeLines(lines, fname)
  expect_true(detect_code(fname))
})

test_that("detect_code detects inline code with trailing spaces", {
  lines <- c("`r 1 + 2 `")
  fname <- tempfile()
  on.exit(unlink(fname))
  writeLines(lines, fname)
  expect_true(detect_code(fname))
})

test_that("detect_code detects inline code that is split across lines", {
  lines <- c("`r 1 +
             2 +
             3`")
  fname <- tempfile()
  on.exit(unlink(fname))
  writeLines(lines, fname)
  expect_true(detect_code(fname))
})

test_that("detect_code isn't fooled by non-inline code", {
  lines <- c("`r`",
             "`r `",
             "`r1`",
             "`r",
             "`")
  fname <- tempfile()
  on.exit(unlink(fname))
  writeLines(lines, fname)
  expect_false(detect_code(fname))
})

test_that("detect_code isn't fooled by non-code chunks", {
  lines <- c("```{",
             "```{}",
             "```{ }",
             "```{r} x")
  fname <- tempfile()
  on.exit(unlink(fname))
  writeLines(lines, fname)
  expect_false(detect_code(fname))
})

test_that("detect_code returns FALSE for file with nothing even resembling code", {
  lines <- c("---",
             "title: \"Untitled\"",
             "---",
             "")
  fname <- tempfile()
  on.exit(unlink(fname))
  writeLines(lines, fname)
  expect_false(detect_code(fname))
})

# Test create_url_html ---------------------------------------------------------

test_that("create_url_html returns CDN for GitHub.com", {
  observed <- workflowr:::create_url_html("https://github.com/user/repo",
                                          "path/file.html", "commit")
  expected <- "<a href=\"https://rawcdn.githack.com/user/repo/commit/path/file.html\" target=\"_blank\">commit</a>"
  expect_identical(observed, expected)

  observed <- workflowr:::create_url_html("https://github.com/jdblischak/dc-bioc-limma",
                                          "docs/index.html", "ef5ea09f1d2e12af8757d2d77a68e16466947a65")
  expected <- "<a href=\"https://rawcdn.githack.com/jdblischak/dc-bioc-limma/ef5ea09f1d2e12af8757d2d77a68e16466947a65/docs/index.html\" target=\"_blank\">ef5ea09</a>"
  expect_identical(observed, expected)
})

test_that("create_url_html returns CDN for GitLab.com", {
  observed <- workflowr:::create_url_html("https://gitlab.com/user/repo",
                                          "path/file.html", "commit")
  expected <- "<a href=\"https://glcdn.githack.com/user/repo/raw/commit/path/file.html\" target=\"_blank\">commit</a>"
  expect_identical(observed, expected)

  observed <- workflowr:::create_url_html("https://gitlab.com/jdblischak/wflow-gitlab",
                                          "public/index.html", "f3b96cfd498ad1b6fb177fa9ae5ad1a2e2ca7261")
  expected <- "<a href=\"https://glcdn.githack.com/jdblischak/wflow-gitlab/raw/f3b96cfd498ad1b6fb177fa9ae5ad1a2e2ca7261/public/index.html\" target=\"_blank\">f3b96cf</a>"
  expect_identical(observed, expected)
})

# https://git.rcc.uchicago.edu/ivy2/Graham_Introduction_to_Hadoop
# https://git.rcc.uchicago.edu/user/repo
test_that("create_url_html returns URL to repo for anything not standard GH/GL", {
  observed <- workflowr:::create_url_html("https://git.rcc.uchicago.edu/user/repo",
                                          "path/file.html", "commit")
  expected <- "<a href=\"https://git.rcc.uchicago.edu/user/repo/blob/commit/path/file.html\" target=\"_blank\">commit</a>"
  expect_identical(observed, expected)
})

# Test shorten_sha -------------------------------------------------------------

test_that("Shorten sha creates 7 character string", {
  observed <- workflowr:::shorten_sha("123456789")
  expected <- "1234567"
  expect_identical(observed, expected)
})

test_that("shorten_sha is vectorized", {
  observed <- workflowr:::shorten_sha(c("123456789", "abcdefghi"))
  expected <- c("1234567", "abcdefg")
  expect_identical(observed, expected)
})
