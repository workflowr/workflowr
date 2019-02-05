context("wflow_html")

# Test wflow_html --------------------------------------------------------------

test_that("wflow_html sets custom knitr chunk options", {

  skip_on_cran()

  # The R Markdown file opts_chunk.Rmd reads the options and exports to an RDS
  # file
  tmp_dir <- tempfile()
  fs::dir_create(tmp_dir)
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  rmd <- file.path(tmp_dir, "file.Rmd")
  fs::file_copy("files/test-wflow_html/opts_chunk.Rmd", rmd)
  html <- render(rmd, quiet = TRUE)
  expect_true(fs::file_exists(html))
  observed <- readRDS(file.path(tmp_dir, "opts_chunk.rds"))
  expect_identical(observed$comment, NA)
  expect_identical(observed$fig.align, "center")
  expect_identical(observed$tidy, FALSE)
})

test_that("wflow_html can set knit_root_dir in YAML header", {

  skip_on_cran()

  # The R Markdown file knit_root_dir.Rmd creates a file knit_root_dir.txt in
  # its working directory, which is one upstream from its file location.
  tmp_dir <- tempfile()
  fs::dir_create(tmp_dir)
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  sub_dir <- file.path(tmp_dir, "sub_dir")
  fs::dir_create(sub_dir)
  rmd <- file.path(sub_dir, "file.Rmd")
  fs::file_copy("files/test-wflow_html/knit_root_dir.Rmd", rmd)
  html <- render(rmd, quiet = TRUE)
  expect_true(fs::file_exists(html))
  expect_false(fs::file_exists(file.path(sub_dir, "knit_root_dir.txt")))
  expect_true(fs::file_exists(file.path(tmp_dir, "knit_root_dir.txt")))
})

test_that("knit_root_dir can be overridden by command-line render argument", {

  skip_on_cran()

  # The R Markdown file knit_root_dir.Rmd creates a file knit_root_dir.txt in
  # its working directory, which is one upstream from its file location.
  # However, this is overriden by passing the directory that contains the file
  # directly to render.
  tmp_dir <- tempfile()
  fs::dir_create(tmp_dir)
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  sub_dir <- file.path(tmp_dir, "sub_dir")
  fs::dir_create(sub_dir)
  rmd <- file.path(sub_dir, "file.Rmd")
  fs::file_copy("files/test-wflow_html/knit_root_dir.Rmd", rmd)
  html <- render(rmd, quiet = TRUE, knit_root_dir = dirname(rmd))
  expect_true(fs::file_exists(html))
  expect_true(fs::file_exists(file.path(sub_dir, "knit_root_dir.txt")))
  expect_false(fs::file_exists(file.path(tmp_dir, "knit_root_dir.txt")))
})

test_that("wflow_html can change the sesssioninfo from the YAML header", {

  skip_on_cran()

  tmp_dir <- tempfile()
  fs::dir_create(tmp_dir)
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  rmd <- file.path(tmp_dir, "file.Rmd")
  lines <- c("---",
             "output: workflowr::wflow_html",
             "workflowr:",
             "  sessioninfo: \"devtools::session_info()\"",
             "---",
             "",
             "`r 1 + 1`")
  writeLines(lines, rmd)
  html <- render(rmd, quiet = TRUE)
  expect_true(fs::file_exists(html))
  html_lines <- readLines(html)
  expect_true(sum(stringr::str_detect(html_lines, "devtools::session_info")) == 1)
})

test_that("wflow_html can change the seed from the YAML header", {

  skip_on_cran()

  tmp_dir <- tempfile()
  fs::dir_create(tmp_dir)
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  rmd <- file.path(tmp_dir, "file.Rmd")
  lines <- c("---",
             "output: workflowr::wflow_html",
             "workflowr:",
             "  seed: 1",
             "---",
             "",
             "`r round(rnorm(1), 5)`")
  writeLines(lines, rmd)
  html <- render(rmd, quiet = TRUE)
  expect_true(fs::file_exists(html))
  html_lines <- readLines(html)
  set.seed(1)
  expect_true(sum(stringr::str_detect(html_lines,
                                      as.character(round(rnorm(1), 5)))) == 1)
})

test_that("wflow_html does not require a YAML header", {

  skip_on_cran()

  tmp_dir <- tempfile()
  fs::dir_create(tmp_dir)
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  rmd <- file.path(tmp_dir, "file.Rmd")
  lines <- c("some text")
  writeLines(lines, rmd)
  html <- render(rmd, quiet = TRUE)
  expect_true(fs::file_exists(html))
  html_lines <- readLines(html)
  expect_true(sum(stringr::str_detect(html_lines, "some text")) == 1)
})

test_that("wflow_html reads _workflowr.yml in the same directory, but can be overidden", {

  skip_on_cran()

  tmp_dir <- tempfile()
  fs::dir_create(tmp_dir)
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Set seed of 5 in _workflowr.yml
  writeLines("seed: 5", con = file.path(tmp_dir, "_workflowr.yml"))

  rmd <- file.path(tmp_dir, "file.Rmd")
  lines <- c("---",
             "output: workflowr::wflow_html",
             "---",
             "",
             "`r round(rnorm(1), 5)`")
  writeLines(lines, rmd)
  html <- render(rmd, quiet = TRUE)
  expect_true(fs::file_exists(html))
  html_lines <- readLines(html)
  set.seed(5)
  expect_true(sum(stringr::str_detect(html_lines,
                                      as.character(round(rnorm(1), 5)))) == 1)

  # Override _workflowr.yml by specifying in YAML header
  lines <- c("---",
             "output: workflowr::wflow_html",
             "workflowr:",
             "  seed: 1",
             "---",
             "",
             "`r round(rnorm(1), 5)`")
  writeLines(lines, rmd)
  html <- render(rmd, quiet = TRUE)
  html_lines <- readLines(html)
  set.seed(1)
  expect_true(sum(stringr::str_detect(html_lines,
                                      as.character(round(rnorm(1), 5)))) == 1)
})

test_that("The default knit_root_dir for a workflowr project is the root directory", {

  skip_on_cran()

  tmp_dir <- tempfile()
  tmp_start <- wflow_start(tmp_dir, change_wd = FALSE, user.name = "Test Name",
                             user.email = "test@email")
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  rmd <- file.path(tmp_dir, "analysis",  "file.Rmd")
  lines <- c("`r getwd()`")
  writeLines(lines, rmd)
  html <- render_site(rmd, quiet = TRUE)
  expect_true(fs::file_exists(html))
  html_lines <- readLines(html)
  expect_true(sum(stringr::str_detect(html_lines, tmp_dir)) == 1)
})

test_that("The default knit_root_dir for a workflowr project can be analysis/", {

  skip_on_cran()

  tmp_dir <- tempfile()
  tmp_start <- wflow_start(tmp_dir, change_wd = FALSE, user.name = "Test Name",
                             user.email = "test@email")
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  wflow_yml <- file.path(tmp_dir, "_workflowr.yml")
  wflow_yml_lines <- readLines(wflow_yml)
  wflow_yml_lines <- stringr::str_replace(wflow_yml_lines,
                                          "knit_root_dir: \".\"",
                                          "knit_root_dir: \"analysis\"")
  writeLines(wflow_yml_lines, wflow_yml)
  rmd <- file.path(tmp_dir, "analysis",  "file.Rmd")
  lines <- c("`r getwd()`")
  writeLines(lines, rmd)
  html <- render_site(rmd, quiet = TRUE)
  expect_true(fs::file_exists(html))
  html_lines <- readLines(html)
  expect_true(sum(stringr::str_detect(html_lines, basename(rmd))) == 1)
})

test_that("wflow_html can insert figures with or without Git repo present", {

  skip_on_cran()

  tmp_dir <- tempfile()
  fs::dir_create(tmp_dir)
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  rmd <- file.path(tmp_dir, "file.Rmd")
  lines <- c("---",
             "output: workflowr::wflow_html",
             "---",
             "",
             "```{r chunkname}",
             "plot(1:10)",
             "```")
  writeLines(lines, rmd)

  # Without Git repo
  html <- render(rmd, quiet = TRUE)
  expect_true(fs::file_exists(html))
  expect_true(fs::file_exists(file.path(tmp_dir, "figure", basename(rmd),
                                    "chunkname-1.png")))
  html_lines <- readLines(html)
  # Because it isn't a website, the image gets embedded as a base64 image
  expect_true(sum(stringr::str_detect(html_lines,
                                      "<img src=\"data:image/png;base64,")) == 1)

  fs::file_delete(html)
  # With Git repo
  git2r::init(tmp_dir)
  html <- render(rmd, quiet = TRUE)
  html_lines <- readLines(html)
  expect_true(sum(stringr::str_detect(html_lines,
                                      "<img src=\"data:image/png;base64,")) == 1)
})

test_that("github URL in _workflowr.yml overrides git remote", {

  skip_on_cran()

  tmp_dir <- tempfile()
  tmp_start <- wflow_start(tmp_dir, change_wd = FALSE, user.name = "Test Name",
                             user.email = "test@email")
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Add remote
  tmp_remote <- wflow_git_remote("origin", "testuser", "testrepo",
                                 verbose = FALSE, project = tmp_dir)

  # Define GitHub URL in _workflowr.yml
  cat("github: https://github.com/upstream/testrepo\n",
      file = file.path(tmp_dir, "_workflowr.yml"), append = TRUE)

  rmd <- file.path(tmp_dir, "analysis", "index.Rmd")
  html <- render_site(rmd, quiet = TRUE)

  html_lines <- readLines(html)
  expect_true(any(stringr::str_detect(html_lines,
                                      "https://github.com/upstream/testrepo")))
  expect_false(any(stringr::str_detect(html_lines,
                                       "https://github.com/testuser/testrepo")))
})

test_that("wflow_html inserts custom header and footer", {

  skip_on_cran()

  tmp_dir <- tempfile()
  fs::dir_create(tmp_dir)
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  rmd <- file.path(tmp_dir, "file.Rmd")
  lines <- c("---",
             "output: workflowr::wflow_html",
             "---")
  writeLines(lines, rmd)
  html <- render(rmd, quiet = TRUE)
  expect_true(fs::file_exists(html))
  html_lines <- readLines(html)
  html_complete <- paste(html_lines, collapse = "\n")
  expect_true(stringr::str_detect(html_complete,
                                  stringr::fixed(workflowr:::includes$header)))
  expect_true(stringr::str_detect(html_complete,
                                  stringr::fixed(workflowr:::includes$footer)))
})

# Test plot_hook ---------------------------------------------------------------

test_that("wflow_html sends warning if fig.path is set by user", {

  skip_on_cran()

  tmp_dir <- tempfile()
  fs::dir_create(tmp_dir)
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # If set in only only one chunk, only one warning should be generated
  rmd <- file.path(tmp_dir, "file.Rmd")
  fs::file_copy("files/test-wflow_html/fig-path-one-chunk.Rmd", rmd)
  html <- render(rmd, quiet = TRUE)
  expect_true(fs::file_exists(html))
  html_lines <- readLines(html)
  expect_true(sum(stringr::str_detect(html_lines, "<code>fig.path</code>")) == 1)

  # If set globally, a warning should be generated for each plot (in this case 3)
  rmd2 <- file.path(tmp_dir, "file2.Rmd")
  fs::file_copy("files/test-wflow_html/fig-path-all-chunks.Rmd", rmd2)
  html2 <- render(rmd2, quiet = TRUE)
  expect_true(fs::file_exists(html2))
  html_lines2 <- readLines(html2)
  expect_true(sum(stringr::str_detect(html_lines2, "<code>fig.path</code>")) == 3)

})

# Test cache_hook --------------------------------------------------------------

test_that("wflow_html sends warning if chunk caches without autodep", {

  skip_on_cran()

  tmp_dir <- tempfile()
  fs::dir_create(tmp_dir)
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # If set in only only one chunk, only one warning should be generated
  #
  # one chunk has cache=TRUE (warning), another has cache=TRUE && autodep=TRUE
  # (no warning), and the third has no options set (no warning).
  rmd <- file.path(tmp_dir, "file.Rmd")
  fs::file_copy("files/test-wflow_html/cache-one-chunk.Rmd", rmd)
  html <- render(rmd, quiet = TRUE)
  expect_true(fs::file_exists(html))
  html_lines <- readLines(html)
  expect_true(sum(stringr::str_detect(html_lines, "<strong>Warning:</strong>")) == 1)

  # If cache=TRUE is set globally, a warning should be generated for each chunk
  # that does not have autodep=TRUE.
  #
  # Expect 3 b/c 1 of 3 chunks has autodep=TRUE, plus added sessioninfo chunk
  # (3 - 1 + 1)
  rmd2 <- file.path(tmp_dir, "file2.Rmd")
  fs::file_copy("files/test-wflow_html/cache-all-chunks.Rmd", rmd2)
  html2 <- render(rmd2, quiet = TRUE)
  expect_true(fs::file_exists(html2))
  html_lines2 <- readLines(html2)
  expect_true(sum(stringr::str_detect(html_lines2, "<strong>Warning:</strong>")) == 3)

})

# Test add_bibliography --------------------------------------------------------

test_that("add_bibliography only adds bibliography when necessary", {
  # Test by directly passing text. The next test block uses actual files
  expected <- c("", "<div id=\"refs\"></div>", "", "")
  expect_identical(workflowr:::add_bibliography("", ""), expected)

  expect_identical(workflowr:::add_bibliography("", "<div id=\"refs\"></div>"),
                   "")

  expect_identical(workflowr:::add_bibliography("", "<div id=\'refs\'></div>"),
                   "")
})

test_that("add_bibliography adds bibliography to files", {

  skip_on_cran()

  tmp_dir <- tempfile()
  fs::dir_create(tmp_dir)
  tmp_dir <- workflowr:::absolute(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Copy test.bib
  fs::file_copy("files/test-wflow_html/test.bib", file.path(tmp_dir, "test.bib"))

  # Don't add bibliography when not specified in YAML header
  bib_none <- file.path(tmp_dir, "bib-none.Rmd")
  fs::file_copy("files/example.Rmd", bib_none)
  bib_none_html <- render(bib_none, quiet = TRUE)
  expect_false(any(stringr::str_detect(readLines(bib_none_html),
                                       "<div id=\"refs\">")))

  # Add bibliography before session information
  bib_add <- file.path(tmp_dir, "bib-add.Rmd")
  fs::file_copy("files/test-wflow_html/bib-add.Rmd", bib_add)
  bib_add_html <- render(bib_add, quiet = TRUE)
  bib_add_lines <- readLines(bib_add_html)
  refs_line <- stringr::str_which(bib_add_lines, "<div id=\"refs\">")
  sinfo_line <- stringr::str_which(bib_add_lines, "sessionInfo()")
  expect_true(refs_line < sinfo_line)

  # Don't add if user already manually added (double quotes)
  bib_dont_add_1 <- file.path(tmp_dir, "bib-dont-add-1.Rmd")
  fs::file_copy("files/test-wflow_html/bib-dont-add-1.Rmd", bib_dont_add_1)
  bib_dont_add_1_html <- render(bib_dont_add_1, quiet = TRUE)
  bib_dont_add_1_lines <- readLines(bib_dont_add_1_html)
  refs_line <- stringr::str_which(bib_dont_add_1_lines, "<div id=\"refs\">")
  expect_true(length(refs_line) == 1)
  sinfo_line <- stringr::str_which(bib_dont_add_1_lines, "sessionInfo()")
  expect_true(refs_line < sinfo_line)

  # Don't add if user already manually added (single quotes)
  bib_dont_add_2 <- file.path(tmp_dir, "bib-dont-add-2.Rmd")
  fs::file_copy("files/test-wflow_html/bib-dont-add-2.Rmd", bib_dont_add_2)
  bib_dont_add_2_html <- render(bib_dont_add_2, quiet = TRUE)
  bib_dont_add_2_lines <- readLines(bib_dont_add_2_html)
  refs_line <- stringr::str_which(bib_dont_add_2_lines, "<div id=[\"\']refs[\"\']>")
  expect_true(length(refs_line) == 1)
  sinfo_line <- stringr::str_which(bib_dont_add_2_lines, "sessionInfo()")
  expect_true(refs_line < sinfo_line)
})
