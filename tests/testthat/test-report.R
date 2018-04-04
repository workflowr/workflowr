context("report")

# Setup ------------------------------------------------------------------------




# Test check_rmd ---------------------------------------------------------------

tmp_dir <- tempfile("test-check_rmd")
dir.create(tmp_dir)
tmp_dir <- workflowr:::absolute(tmp_dir)
git2r::init(tmp_dir)
r <- git2r::repository(tmp_dir)
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
