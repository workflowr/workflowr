context("report")

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
  github <- workflowr:::get_github_from_remote(tmp_dir)
  versions <- workflowr:::get_versions(input = rmd, output_dir, blobs, r, github)
  expect_true(any(stringr::str_detect(versions, github)))
  fig <- file.path(output_dir, "figure", basename(rmd), "chunkname-1.png")
  versions_fig <- get_versions_fig(fig, r, github)
  expect_true(any(stringr::str_detect(versions_fig, github)))
})


# Test check_vc ----------------------------------------------------------------

tmp_dir <- tempfile("test-check_vc")
dir.create(tmp_dir)
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

git2r::add(r, rmd)
git2r::commit(r, "Add rmd")
s <- git2r::status(r, ignored = TRUE)
current_commit <- git2r::commits(r)[[1]]@sha
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
  file.create(fname_html)
  fname_png <- file.path(tmp_dir, "file.png")
  file.create(fname_png)
  site_libs <- file.path(tmp_dir, "site_libs")
  dir.create(site_libs)
  site_libs_readme <- file.path(site_libs, "README.md")
  file.create(site_libs_readme)

  observed <- workflowr:::check_vc(rmd, r = r, s = s, github = NA_character_)

  expect_true(observed$pass)
  expect_false(grepl(basename(fname_html), observed$details))
  expect_false(grepl(basename(fname_png), observed$details))
  expect_false(grepl(basename(site_libs), observed$details))
  expect_true(grepl("working directory clean", observed$details))
})

rmd2 <- file.path(tmp_dir, "file2.Rmd")
file.create(rmd2)
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
file.copy("files/example.Rmd", rmd)

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

# Test check_rmd ---------------------------------------------------------------

tmp_dir <- tempfile("test-check_rmd")
dir.create(tmp_dir)
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

git2r::add(r, rmd)
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
