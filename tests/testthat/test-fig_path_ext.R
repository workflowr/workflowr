context("fig_path_ext")

# Scenario #1: explicitly set fig_path_ext to FALSE ----------------------------

site_dir <- tempfile("test-fig_path_ext-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE, user.name = "Test Name",
                             user.email = "test@email"))
on.exit(unlink(site_dir, recursive = TRUE, force = TRUE))
site_dir <- workflowr:::absolute(site_dir)

s <- wflow_status(project = site_dir)

rmd <- rownames(s$status)
stopifnot(length(rmd) > 0)
# Expected html files
html <- workflowr:::to_html(rmd, outdir = s$docs)

# Explicitly set fig_path_ext to FALSE
cat("fig_path_ext: FALSE\n", file = file.path(s$root, "_workflowr.yml"),
    append = TRUE)

test_that("fig_path_ext has no effect when set to FALSE", {
  # Confirm the setting is FALSE
  wflow_yml <- file.path(s$root, "_workflowr.yml")
  wflow_yml_opts <- yaml::yaml.load_file(wflow_yml)
  expect_false(wflow_yml_opts$fig_path_ext)
  wflow_opts <- workflowr:::wflow_options(rmd[1])
  expect_false(wflow_opts$fig_path_ext)

  # Figure functions detect that the setting is FALSE
  expect_false(workflowr:::is_fig_path_ext(rmd[1]))
  expect_identical(
    stringr::str_sub(workflowr:::create_figure_path(rmd[1]), start = -4),
    ".Rmd")

  skip_on_cran()

  # Publish a file with figures
  file_w_figs <- file.path(s$analysis, "fig.Rmd")
  fs::file_copy("files/test-wflow_build/figure-v01.Rmd", file_w_figs)
  published <- wflow_publish(file_w_figs, "A file with figures", view = FALSE,
                             project = site_dir)
  docs_fig <- file.path(s$docs, "figure", basename(file_w_figs), "unnamed-chunk-1-1.png")
  expect_true(docs_fig %in% published$step3$commit_files)
  analysis_fig <- file.path(s$analysis, "figure", basename(file_w_figs), "unnamed-chunk-1-1.png")
  expect_false(fs::file_exists(analysis_fig))

  # Remove a file with figures
  removed <- wflow_remove(file_w_figs, "Remove file", project = site_dir)
  expect_true(docs_fig %in% removed$files_git)
  expect_false(fs::dir_exists(dirname(docs_fig)))
})

# Scenario #2: set fig_path_ext to TRUE ----------------------------------------

site_dir <- tempfile("test-fig_path_ext-")
suppressMessages(wflow_start(site_dir, change_wd = FALSE, user.name = "Test Name",
                             user.email = "test@email"))
on.exit(unlink(site_dir, recursive = TRUE, force = TRUE), add = TRUE)
site_dir <- workflowr:::absolute(site_dir)

s <- wflow_status(project = site_dir)

rmd <- rownames(s$status)
stopifnot(length(rmd) > 0)
# Expected html files
html <- workflowr:::to_html(rmd, outdir = s$docs)

# Set fig_path_ext to TRUE
cat("fig_path_ext: TRUE\n", file = file.path(s$root, "_workflowr.yml"),
    append = TRUE)

test_that("fig_path_ext removes file extension in fig dir when set to FALSE", {
  # Confirm the setting is TRUE
  wflow_yml <- file.path(s$root, "_workflowr.yml")
  wflow_yml_opts <- yaml::yaml.load_file(wflow_yml)
  expect_true(wflow_yml_opts$fig_path_ext)
  wflow_opts <- workflowr:::wflow_options(rmd[1])
  expect_true(wflow_opts$fig_path_ext)

  # Figure functions detect that the setting is FALSE
  expect_true(workflowr:::is_fig_path_ext(rmd[1]))
  expect_identical(
    basename(workflowr:::create_figure_path(rmd[1])),
    basename(tools::file_path_sans_ext(rmd[1])))

  skip_on_cran()

  # Publish a file with figures
  file_w_figs <- file.path(s$analysis, "fig.Rmd")
  fs::file_copy("files/test-wflow_build/figure-v01.Rmd", file_w_figs)
  published <- wflow_publish(file_w_figs, "A file with figures", view = FALSE,
                             project = site_dir)
  docs_fig <- file.path(s$docs, "figure",
                        tools::file_path_sans_ext(basename(file_w_figs)),
                        "unnamed-chunk-1-1.png")
  expect_true(docs_fig %in% published$step3$commit_files)
  analysis_fig <- file.path(s$analysis, "figure",
                            tools::file_path_sans_ext(basename(file_w_figs)),
                            "unnamed-chunk-1-1.png")
  expect_false(fs::file_exists(analysis_fig))

  # Remove a file with figures
  removed <- wflow_remove(file_w_figs, "Remove file", project = site_dir)
  expect_true(docs_fig %in% removed$files_git)
  expect_false(fs::dir_exists(dirname(docs_fig)))
})
