context("report")

# Setup ------------------------------------------------------------------------

source("setup.R")

skip_on_cran_windows()

# On some machines, the timezone isn't set. This is likely to be a test
# environment, and thus can just use UTC.
timezone <- Sys.timezone()
if (is.na(timezone) || identical(timezone, "")) timezone <- "Etc/UTC"

# Test get_versions_df ---------------------------------------------------------

test_that("Conversion of git_time to character string of date is correct", {

  path <- fs::file_temp()
  fs::dir_create(path)
  on.exit(test_teardown(path))
  r <- git2r::init(path)
  git2r::config(r, user.name = "Test User", user.email = "testing")

  f1 <- file.path(path, "f1.txt")
  cat("line 1\n", file = f1)
  git2r::add(r, f1)
  c1 <- git2r::commit(r, "The first commit to f1")

  expect_identical(
    as.character(Sys.Date()),
    as.character(as.Date(as.POSIXct(c1$author$when), tz = timezone))
  )
})

test_that("get_versions_df returns data frame of commits for file(s)", {

  path <- fs::file_temp()
  fs::dir_create(path)
  on.exit(test_teardown(path))
  r <- git2r::init(path)
  git2r::config(r, user.name = "Test User", user.email = "testing")

  f1 <- file.path(path, "f1.txt")
  f2 <- file.path(path, "f2.txt")

  cat("line 1\n", file = f1)
  git2r::add(r, f1)
  c1 <- git2r::commit(r, "The first commit to f1")
  Sys.sleep(1)

  cat("line 1\n", file = f2)
  git2r::add(r, f2)
  c2 <- git2r::commit(r, "The first commit to f2")
  Sys.sleep(1)

  cat("line 2\n", file = f1, append = TRUE)
  git2r::add(r, f1)
  c3 <- git2r::commit(r, "The second commit to f1")
  Sys.sleep(1)

  cat("line 2\n", file = f2, append = TRUE)
  git2r::add(r, f2)
  c4 <- git2r::commit(r, "The second commit to f2")
  Sys.sleep(1)

  versions_f1 <- workflowr:::get_versions_df("f1.txt", r)

  expect_true(all(versions_f1$File == "f1.txt"))
  expect_identical(versions_f1$Version, c(c3$sha, c1$sha))
  expect_true(all(versions_f1$Author == "Test User"))
  expect_identical(
    versions_f1$Date,
    as.character(c(as.Date(as.POSIXct(c3$author$when), tz = timezone),
                   as.Date(as.POSIXct(c1$author$when), tz = timezone)))
  )
  expect_identical(versions_f1$Message, c(c3$message, c1$message))

  versions_f2 <- workflowr:::get_versions_df("f2.txt", r)

  expect_true(all(versions_f2$File == "f2.txt"))
  expect_identical(versions_f2$Version, c(c4$sha, c2$sha))
  expect_true(all(versions_f2$Author == "Test User"))
  expect_identical(
    versions_f2$Date,
    as.character(c(as.Date(as.POSIXct(c4$author$when), tz = timezone),
                   as.Date(as.POSIXct(c2$author$when), tz = timezone)))
  )
  expect_identical(versions_f2$Message, c(c4$message, c2$message))

  versions_f1_f2 <- workflowr:::get_versions_df(c("f1.txt", "f2.txt"), r)

  expect_true(all(versions_f1_f2$File == c("f2.txt", "f1.txt", "f2.txt", "f1.txt")))
  expect_identical(versions_f1_f2$Version, c(c4$sha, c3$sha, c2$sha, c1$sha))
  expect_true(all(versions_f1_f2$Author == "Test User"))
  expect_identical(
    versions_f1_f2$Date,
    as.character(c(as.Date(as.POSIXct(c4$author$when), tz = timezone),
                   as.Date(as.POSIXct(c3$author$when), tz = timezone),
                   as.Date(as.POSIXct(c2$author$when), tz = timezone),
                   as.Date(as.POSIXct(c1$author$when), tz = timezone)))
  )
  expect_identical(versions_f1_f2$Message, c(c4$message, c3$message,
                                             c2$message, c1$message))

  # Reversing the input file order should have no effect
  versions_f2_f1 <- workflowr:::get_versions_df(c("f2.txt", "f1.txt"), r)
  expect_identical(versions_f2_f1, versions_f1_f2)

  expect_identical(workflowr:::get_versions_df("non-existent", r), data.frame())

  expect_silent(workflowr:::get_versions_df(f1, r))
})


test_that("get_versions_df uses Etc/UTC if timezone isn't set", {

  path <- fs::file_temp()
  fs::dir_create(path)
  on.exit(test_teardown(path))
  r <- git2r::init(path)
  git2r::config(r, user.name = "Test User", user.email = "testing")

  f1 <- file.path(path, "f1.txt")

  cat("line 1\n", file = f1)
  git2r::add(r, f1)
  c1 <- git2r::commit(r, "The first commit to f1")
  Sys.sleep(1)

  cat("line 2\n", file = f1, append = TRUE)
  git2r::add(r, f1)
  c2 <- git2r::commit(r, "The second commit to f1")

  versions_null <- workflowr:::get_versions_df("f1.txt", r, timezone = NULL)
  expect_identical(
    versions_null$Date,
    as.character(c(as.Date(as.POSIXct(c2$author$when), tz = "Etc/UTC"),
                   as.Date(as.POSIXct(c1$author$when), tz = "Etc/UTC")))
  )

  versions_na <- workflowr:::get_versions_df("f1.txt", r, timezone = NA)
  expect_identical(
    versions_na$Date,
    as.character(c(as.Date(as.POSIXct(c2$author$when), tz = "Etc/UTC"),
                   as.Date(as.POSIXct(c1$author$when), tz = "Etc/UTC")))
  )

  versions_nac <- workflowr:::get_versions_df("f1.txt", r, timezone = NA_character_)
  expect_identical(
    versions_nac$Date,
    as.character(c(as.Date(as.POSIXct(c2$author$when), tz = "Etc/UTC"),
                   as.Date(as.POSIXct(c1$author$when), tz = "Etc/UTC")))
  )

  versions_empty <- workflowr:::get_versions_df("f1.txt", r, timezone = "")
  expect_identical(
    versions_null$Date,
    as.character(c(as.Date(as.POSIXct(c2$author$when), tz = "Etc/UTC"),
                   as.Date(as.POSIXct(c1$author$when), tz = "Etc/UTC")))
  )
})

# Test get_versions and get_versions_fig ---------------------------------------

test_that("get_versions inserts table of past versions", {

  skip_on_cran()

  path <- test_setup()
  on.exit(test_teardown(path))

  rmd <- file.path(path, "analysis", "index.Rmd")
  for (i in 1:2) wflow_publish(rmd, view = FALSE, project = path)

  html <- workflowr:::to_html(rmd, outdir = file.path(path, "docs"))
  html_lines <- readLines(html)
  expect_false(any(
    stringr::str_detect(html_lines, "There are no past versions")
  ))
  expect_true(any(
    stringr::str_detect(html_lines, "These are the previous versions")
  ))
})

test_that("get_versions and get_versions_fig insert GitHub URL if available", {

  skip_on_cran()

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  rmd <- file.path(path, "analysis", "file.Rmd")
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
                                 verbose = FALSE, project = path)

  # Go through a few commit cycles
  for (i in 1:3) {
    cat("edit", file = rmd, append = TRUE)
    tmp_publish <- wflow_publish(rmd, view = FALSE, project = path)
  }

  r <- git2r::repository(path)
  output_dir <- workflowr:::get_output_dir(file.path(path, "analysis/"))
  github <- workflowr:::get_host_from_remote(path)
  versions <- workflowr:::get_versions(input = rmd, output_dir, r, github)
  expect_true(any(stringr::str_detect(versions, github)))
  fig <- file.path(output_dir, "figure", basename(rmd), "chunkname-1.png")
  versions_fig <- workflowr:::get_versions_fig(fig, r, github)
  expect_true(any(stringr::str_detect(versions_fig, github)))
})

test_that("get_versions_fig converts spaces to dashes for HTML ID", {

  skip_on_cran()

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  rmd <- file.path(path, "analysis", "file.Rmd")
  lines <- c("---",
             "output: workflowr::wflow_html",
             "---",
             "",
             "```{r chunk-name}",
             "plot(1:10)",
             "```",
             "",
             "```{r chunk name}",
             "plot(11:20)",
             "```")
  writeLines(lines, rmd)

  # Add remote
  tmp_remote <- wflow_git_remote("origin", "testuser", "testrepo",
                                 verbose = FALSE, project = path)

  # Commit twice so that past versions table is generated
  tmp_publish <- wflow_publish(rmd, view = FALSE, project = path)
  tmp_publish <- wflow_publish(rmd, view = FALSE, project = path)

  r <- git2r::repository(path)
  output_dir <- workflowr:::get_output_dir(file.path(path, "analysis/"))
  github <- workflowr:::get_host_from_remote(path)

  # The figure file without spaces should be displayed as normal
  fig <- file.path(output_dir, "figure", basename(rmd), "chunk-name-1.png")
  versions_fig <- workflowr:::get_versions_fig(fig, r, github)
  versions_fig_lines <- stringr::str_split(versions_fig, "\\n")[[1]]
  data_target <- stringr::str_subset(versions_fig_lines,
                                      'data-target=\"#fig-chunk-name-1\"')
  expect_true(length(data_target) == 1)
  div_id <- stringr::str_subset(versions_fig_lines,
                                'id=\"fig-chunk-name-1\"')
  expect_true(length(div_id) == 1)
  fig_display_name <- stringr::str_subset(versions_fig_lines,
                                          ' chunk-name-1.png')
  expect_true(length(fig_display_name) == 1)

  # The figure file with spaces should be quoted and have spaces replaced with
  # dashes for data-target and id.
  fig <- file.path(output_dir, "figure", basename(rmd), "chunk name-1.png")
  versions_fig <- workflowr:::get_versions_fig(fig, r, github)
  versions_fig_lines <- stringr::str_split(versions_fig, "\\n")[[1]]
  data_target <- stringr::str_subset(versions_fig_lines,
                                     'data-target=\"#fig-no-spaces-chunk-name-1\"')
  expect_true(length(data_target) == 1)
  div_id <- stringr::str_subset(versions_fig_lines,
                                'id=\"fig-no-spaces-chunk-name-1\"')
  expect_true(length(div_id) == 1)
  fig_display_name <- stringr::str_subset(versions_fig_lines,
                                          ' &quot;chunk name-1.png&quot;')
  expect_true(length(fig_display_name) == 1)
})

test_that("get_versions_fig produces figure version table", {

  skip_on_cran()

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  rmd <- file.path(path, "analysis", "file.Rmd")
  lines <- c("---",
             "output: workflowr::wflow_html",
             "---",
             "",
             "```{r chunk-name}",
             "plot(1:10)",
             "```")
  writeLines(lines, rmd)

  # Add remote
  tmp_remote <- wflow_git_remote("origin", "testuser", "testrepo",
                                 verbose = FALSE, project = path)

  # Commit once
  tmp_publish <- wflow_publish(rmd, view = FALSE, project = path)

  # Change the plot
  lines <- c("---",
             "output: workflowr::wflow_html",
             "---",
             "",
             "```{r chunk-name}",
             "plot(2:10)",
             "```")
  writeLines(lines, rmd)

  # Commit again with a modified plot so that the figure version table is generated
  tmp_publish <- wflow_publish(rmd, view = FALSE, project = path)

  output_dir <- workflowr:::get_output_dir(file.path(path, "analysis/"))

  # The figure file without spaces should be displayed as normal
  output_html <- file.path(output_dir, "file.html")
  output_html_content <- readLines(output_html)
  expect_true(any(stringr::str_detect(output_html_content, "Past versions of chunk-name-1.png")))
})

# Test check_vc ----------------------------------------------------------------

tmp_dir <- tempfile("test-check_vc")
fs::dir_create(tmp_dir)
tmp_dir <- workflowr:::absolute(tmp_dir)
rmd <- file.path(tmp_dir, "file.Rmd")
writeLines(letters, rmd)
output_dir <- file.path(tmp_dir, "website")
fs::dir_create(output_dir)

test_that("check_vc reports lack of Git repo", {
  observed <- workflowr:::check_vc(rmd, r = NULL, s = NULL,
                                   github = NA_character_, output_dir = output_dir)
  expect_false(observed$pass)
  expect_identical(observed$summary,
                   "<strong>Repository version:</strong> no version control")
})

git2r::init(tmp_dir)
r <- git2r::repository(tmp_dir)
git2r::config(r, user.name = "Test Name", user.email = "test@email")
s <- git2r::status(r, ignored = TRUE)

test_that("check_vc reports Git repo even if no commits", {
  observed <- workflowr:::check_vc(rmd, r = r, s = s, github = NA_character_,
                                   output_dir = output_dir)
  expect_true(observed$pass)
  expect_identical(observed$summary,
                   "<strong>Repository version:</strong> No commits yet")
})

workflowr:::git2r_add(r, rmd)
git2r::commit(r, "Add rmd")
s <- git2r::status(r, ignored = TRUE)
current_commit <- git2r::commits(r)[[1]]$sha
commit_to_display <- workflowr:::shorten_sha(current_commit)

test_that("check_vc reports Git repo", {
  observed <- workflowr:::check_vc(rmd, r = r, s = s, github = NA_character_,
                                   output_dir = output_dir)
  expect_true(observed$pass)
  expect_identical(observed$summary,
                   sprintf("<strong>Repository version:</strong> %s",
                           commit_to_display))
})

test_that("check_vc reports Git repo and can add GitHub URL", {
  github <- "https://github.com/workflowr/workflowr"
  observed <- workflowr:::check_vc(rmd, r = r, s = s, github = github,
                                   output_dir = output_dir)
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

  observed <- workflowr:::check_vc(rmd, r = r, s = s, github = NA_character_,
                                   output_dir = output_dir)

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
  observed <- workflowr:::check_vc(rmd, r = r, s = s, github = NA_character_,
                                   output_dir = output_dir)

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

test_that("check_sessioninfo reports sessioninfo in file as session_info() from sessioninfo", {
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
  fs::dir_create(rmd_cache_html)

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
               fig_path_ext = FALSE, suppress_report = FALSE)

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

  # Suppress the reports
  opts$suppress_report <- TRUE
  report <- create_report(input, output_dir, has_code, opts)
  expected <- '<div id="workflowr-report" class="collapse">'
  expect_false(stringr::str_detect(report, expected))
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

# Test workflowr:::detect_abs_path ---------------------------------------------------------

test_that("workflowr:::detect_abs_path detects absolute file paths in quotations", {
  # double quotes
  expect_identical(
    workflowr:::detect_abs_path(c("x <- readLines(\"/home/jdb-work/repos/workflowr/R/git.R\")",
                      "files <- c(\"/a/b/c\", \"/d/e/f\", \"/h/i/j\")")),
    c("/home/jdb-work/repos/workflowr/R/git.R", "/a/b/c", "/d/e/f", "/h/i/j"))

  # single quotes
  expect_identical(
    workflowr:::detect_abs_path(c("x <- readLines('/home/jdb-work/repos/workflowr/R/git.R\')",
                      "files <- c('/a/b/c', '/d/e/f', '/h/i/j')")),
    c("/home/jdb-work/repos/workflowr/R/git.R", "/a/b/c", "/d/e/f", "/h/i/j"))

})

test_that("workflowr:::detect_abs_path ignores relative paths", {
  expect_identical(
    workflowr:::detect_abs_path(c("x <- readLines(\"R/git.R\")",
                      "files <- c(\"a/b/c\", \"./d/e/f\", \"../h/i/j\")")),
    character(0))
})

test_that("workflowr:::detect_abs_path detects absolute file paths with tilde in quotations", {
  # double quotes
  expect_identical(
    workflowr:::detect_abs_path(c("x <- readLines(\"/home/jdb-work/repos/workflowr/R/git.R\")",
                      "files <- c(\"/a/b/c\", \"/d/e/f\", \"/h/i/j\")")),
    c("/home/jdb-work/repos/workflowr/R/git.R", "/a/b/c", "/d/e/f", "/h/i/j"))

  # single quotes
  expect_identical(
    workflowr:::detect_abs_path(c("x <- readLines('/home/jdb-work/repos/workflowr/R/git.R\')",
                      "files <- c('/a/b/c', '/d/e/f', '/h/i/j')")),
    c("/home/jdb-work/repos/workflowr/R/git.R", "/a/b/c", "/d/e/f", "/h/i/j"))

})

test_that("workflowr:::detect_abs_path detects Windows absolute file paths in quotations", {
  # double quotes
  expect_identical(
    workflowr:::detect_abs_path(c("x <- readLines(\"C:/home/jdb-work/repos/workflowr/R/git.R\")",
                      "files <- c(\"C:/a/b/c\", \"c:\\d\\e\\f\", \"D:/h/i/j\")")),
    c("C:/home/jdb-work/repos/workflowr/R/git.R", "C:/a/b/c", "c:\\d\\e\\f", "D:/h/i/j"))

  # single quotes
  expect_identical(
    workflowr:::detect_abs_path(c("x <- readLines('/home/jdb-work/repos/workflowr/R/git.R\')",
                      "files <- c('/a/b/c', '/d/e/f', '/h/i/j')")),
    c("/home/jdb-work/repos/workflowr/R/git.R", "/a/b/c", "/d/e/f", "/h/i/j"))

})

test_that("workflowr:::detect_abs_path ignores non-paths", {
  expect_identical(
    workflowr:::detect_abs_path(c("the volume is about \"~1 mL\"",
                      "Just a letter and a colon 'a:'")),
    character(0))
})

test_that("workflowr:::detect_abs_path ignores non-paths", {
  expect_identical(
    workflowr:::detect_abs_path(c("the volume is about \"~1 mL\"",
                                  "Just a letter and a colon 'a:'")),
    character(0))
})

test_that("workflowr:::detect_abs_path distinguishes paths from non-paths", {
  # Using my particular definition of paths to detect (e.g. has to be in
  # quotation marks)

  expect_setequal(
    workflowr:::detect_abs_path(
      c("\"/a/b/c\"", "'/a/b/c'", "\"~/a/b/c\"", "\"~\\a\\b\\c\"", "\"C:/a/b/c\"", "\"C:\\a\\b\\c\"")),
    c("/a/b/c", "/a/b/c", "~/a/b/c", "~\\a\\b\\c", "C:/a/b/c", "C:\\a\\b\\c"))

  # Non-paths to ignore
  expect_identical(
    workflowr:::detect_abs_path(c("/a/b/c", "\"~a\"", "\"C:a/b/c\"", "\"~\"")),
    character(0))

})


# Test get_proj_dir ------------------------------------------------------------

test_that("get_proj_dir find project directory", {

  path <- fs::file_temp()
  fs::dir_create(path)
  on.exit(test_teardown(path))

  # Create nested subdirectories, and progressively add root files
  subdir1 <- file.path(path, "subdir1")
  subdir2 <- file.path(subdir1, "subdir2")
  subdir3 <- file.path(subdir2, "subdir3")
  subdir4 <- file.path(subdir3, "subdir4")
  fs::dir_create(subdir4)
  # Resolve macOS symlinks now that directories exist
  subdir1 <- workflowr:::absolute(subdir1)
  subdir2 <- workflowr:::absolute(subdir2)
  subdir3 <- workflowr:::absolute(subdir3)
  subdir4 <- workflowr:::absolute(subdir4)

  # If no proj files, return same directory
  expected <- subdir4
  observed <- workflowr:::get_proj_dir(subdir4)
  expect_identical(observed, expected)

  # If _workflowr.yml in subdir1, return subdir1
  wflow_yml_file <- file.path(subdir1, "_workflowr.yml")
  fs::file_create(wflow_yml_file)
  expected <- subdir1
  observed <- workflowr:::get_proj_dir(subdir4)
  expect_identical(observed, expected)

  # If .git/ in subdir2, return subdir2
  git_dir <- file.path(subdir2, ".git/")
  fs::dir_create(git_dir)
  expected <- subdir2
  observed <- workflowr:::get_proj_dir(subdir4)
  expect_identical(observed, expected)

  # If *Rproj in subdir3, return subdir3
  rproj_file <- file.path(subdir3, "subdir3.Rproj")
  fs::file_create(rproj_file)
  # Criteria includes "contents matching `^Version: ` in the first line"
  writeLines("Version: ", con = rproj_file)
  expected <- subdir3
  observed <- workflowr:::get_proj_dir(subdir4)
  expect_identical(observed, expected)
})

# Test check_paths -------------------------------------------------------------

test_that("check_paths detects absolute paths", {

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  f <- file.path(path, "data/test.txt")

  rmd <- file.path(path, "analysis", "file.Rmd")
  lines <- glue::glue("
                      ---
                      output: workflowr::wflow_html
                      ---

                      ```{{r chunkname}}
                      x <- read.table(\"{f}\")
                      ```")
  writeLines(lines, rmd)

  observed <- workflowr:::check_paths(input = rmd, knit_root_dir = path)
  expect_false(observed$pass)
  expect_true(stringr::str_detect(observed$details, "\\bdata/test.txt\\b"))
})

test_that("check_paths ignores relative paths", {

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  f <- file.path(path, "data/test.txt")
  f <- workflowr:::relative(f, start = path)

  rmd <- file.path(path, "analysis", "file.Rmd")
  lines <- glue::glue("
                      ---
                      output: workflowr::wflow_html
                      ---

                      ```{{r chunkname}}
                      x <- read.table(\"{f}\")
                      ```")
  writeLines(lines, rmd)

  observed <- workflowr:::check_paths(input = rmd, knit_root_dir = path)
  expect_true(observed$pass)
})

test_that("check_paths suggests paths relative to knit_root_dir", {

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  f_data <- file.path(path, "data/test.txt")
  f_html <- file.path(path, "docs/index.html")
  f_code <- file.path(path, "analysis/code.R")

  rmd <- file.path(path, "analysis", "file.Rmd")
  lines <- glue::glue("
                      ---
                      output: workflowr::wflow_html
                      ---

                      [Home]({f_html})

                      ```{{r chunkname}}
                      source(\"{f_code}\")
                      x <- read.table(\"{f_data}\")
                      ```
                      ")
  writeLines(lines, rmd)

  observed <- workflowr:::check_paths(input = rmd, knit_root_dir = path)
  expect_false(observed$pass)
  expect_true(stringr::str_detect(observed$details, " data/test.txt "))
  expect_false(stringr::str_detect(observed$details, " docs/index.html "))
  expect_true(stringr::str_detect(observed$details, " analysis/code.R "))
  # Change knit_root_dir to analysis
  observed <- workflowr:::check_paths(input = rmd,
                                      knit_root_dir = fs::path_dir(rmd))
  expect_false(observed$pass)
  expect_true(stringr::str_detect(observed$details, " \\.\\./data/test.txt "))
  expect_false(stringr::str_detect(observed$details, " \\.\\./docs/index.html "))
  expect_true(stringr::str_detect(observed$details, " code.R "))
})

test_that("check_paths displays original formatting of paths", {

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  f_data <- file.path(path, "data/test.txt")
  # Add extra trailing slashes
  f_data <- paste0(f_data, "////")

  rmd <- file.path(path, "analysis", "file.Rmd")
  lines <- glue::glue("
                      ---
                      output: workflowr::wflow_html
                      ---

                      ```{{r chunkname}}
                      x <- read.table(\"{f_data}\")
                      ```
                      ")
  writeLines(lines, rmd)

  observed <- workflowr:::check_paths(input = rmd, knit_root_dir = path)
  expect_false(observed$pass)
  expect_true(stringr::str_detect(observed$details, f_data))
  expect_true(stringr::str_detect(observed$details, " data/test.txt "))
})

test_that("check_paths displays original formatting of Windows paths", {

  if (.Platform$OS.type != "windows") skip("Only relevant on Windows")

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  f_data <- file.path(path, "data/test.txt")
  # Make the drive lowercase
  f_data <- paste0(tolower(stringr::str_sub(f_data, 1, 1)),
                   stringr::str_sub(f_data, 2))
  f_code <- file.path(path, "analysis/code.R")
  # Use backslashes (getting it to actually display \\ in the Rmd is a pain)
  f_code <- stringr::str_replace_all(f_code, "/", "\\\\\\\\")

  rmd <- file.path(path, "analysis", "file.Rmd")
  lines <- glue::glue("
                      ---
                      output: workflowr::wflow_html
                      ---

                      ```{{r chunkname, eval=FALSE}}
                      source(\"{f_code}\")
                      x <- read.table(\"{f_data}\")
                      ```
                      ")
  writeLines(lines, rmd)

  observed <- workflowr:::check_paths(input = rmd, knit_root_dir = path)
  expect_false(observed$pass)
  expect_true(stringr::str_detect(observed$details, f_data))
  expect_true(stringr::str_detect(observed$details, " data/test.txt "))
  # Because using \ in a regex is a pain, always have to double it
  f_code_regex <- stringr::str_replace_all(f_code, "\\\\", "\\\\\\\\\\\\\\\\")
  expect_true(stringr::str_detect(observed$details, f_code_regex))
  expect_true(stringr::str_detect(observed$details, " analysis\\\\\\\\\\\\\\\\code.R "))

  skip_on_cran()

  # Make sure wflow_html() can render this (not actually execute the code chunk
  # though, since those files don't exist). I've had some sporadic pandoc errors
  # that are difficult to reproduce.
  expect_silent(html <- rmarkdown::render(rmd, quiet = TRUE))
  expect_true(fs::file_exists(html))
})

# Test scrub_status ------------------------------------------------------------

status_empty <- structure(
  list(staged = structure(list(), .Names = character(0)),
       unstaged = structure(list(), .Names = character(0)),
       untracked = structure(list(), .Names = character(0))),
  class = "git_status")




test_that("scrub_status can return a clean working directory", {

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  r <- git2r::repository(path = path)
  s <- git2r::status(r)
  output_dir <- workflowr:::wflow_paths(project = path)$docs

  observed <- workflowr:::scrub_status(s, r, output_dir = output_dir)
  expected <- s
  expect_identical(observed, expected)
})

test_that("scrub_status scrubs website directory when run outside project", {

  skip_on_cran()

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  r <- git2r::repository(path = path)
  output_dir <- workflowr:::wflow_paths(project = path)$docs
  wflow_build(view = FALSE, project = path)
  s <- git2r::status(r)

  observed <- workflowr:::scrub_status(s, r, output_dir = output_dir)
  expected <- status_empty
  expect_identical(observed, expected)
})

test_that("scrub_status scrubs website directory when run from project root", {

  skip_on_cran()

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  withr::local_dir(path)

  r <- git2r::repository()
  output_dir <- workflowr:::wflow_paths()$docs
  wflow_build(view = FALSE)
  s <- git2r::status(r)

  observed <- workflowr:::scrub_status(s, r, output_dir = output_dir)
  expected <- status_empty
  expect_identical(observed, expected)
})

test_that("scrub_status scrubs website directory when run from project subdir", {

  skip_on_cran()

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  withr::local_dir(file.path(path, "analysis"))

  r <- git2r::repository()
  output_dir <- workflowr:::wflow_paths()$docs
  wflow_build(view = FALSE)
  s <- git2r::status(r)

  observed <- workflowr:::scrub_status(s, r, output_dir = output_dir)
  expected <- status_empty
  expect_identical(observed, expected)
})

test_that("scrub_status scrubs website directory when run from website subdir", {

  skip_on_cran()

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  withr::local_dir(file.path(path, "docs"))

  r <- git2r::repository()
  output_dir <- workflowr:::wflow_paths()$docs
  wflow_build(view = FALSE)
  s <- git2r::status(r)

  observed <- workflowr:::scrub_status(s, r, output_dir = output_dir)
  expected <- status_empty
  expect_identical(observed, expected)
})


test_that("scrub_status can remove ignored files", {

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  r <- git2r::repository(path = path)
  output_dir <- workflowr:::wflow_paths(project = path)$docs

  ignored <- file.path(path, "ignored.txt")
  fs::file_create(ignored)
  cat("ignored*\n", file = file.path(path, ".gitignore"), append = TRUE)
  s <- git2r::status(r, ignored = TRUE)

  observed_include_ignore <- workflowr:::scrub_status(s, r, output_dir = output_dir)
  expected_include_ignore <- s
  expect_identical(observed_include_ignore, expected_include_ignore)

  observed_remove_ignore <- workflowr:::scrub_status(s, r, output_dir = output_dir,
                                                     remove_ignored = TRUE)
  expected_remove_ignore <- structure(
    list(staged = structure(list(), .Names = character(0)),
    unstaged = list(modified = ".gitignore"),
    untracked = structure(list(), .Names = character(0))),
    class = "git_status")
  expect_identical(observed_remove_ignore, expected_remove_ignore)
})

test_that("scrub_status preserves non-website files", {

  skip_on_cran()

  # Setup functions from setup.R
  path <- test_setup()
  on.exit(test_teardown(path))

  r <- git2r::repository(path = path)
  output_dir <- workflowr:::wflow_paths(project = path)$docs

  index <- file.path(path, "analysis", "index.Rmd")
  cat("edit\n", file = index, append = TRUE)
  wflow_build(index, view = FALSE, project = path)
  s <- git2r::status(r)

  observed <- workflowr:::scrub_status(s, r, output_dir = output_dir)
  expected <- structure(
    list(staged = structure(list(), .Names = character(0)),
    unstaged = list(modified = "analysis/index.Rmd"),
    untracked = structure(list(), .Names = character(0))),
    class = "git_status")
  expect_identical(observed, expected)
})
