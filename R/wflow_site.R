
#' @export
wflow_site <- function(input, encoding = getOption("encoding"), ...) {

  render <- function(input_file,
                     output_format,
                     envir,
                     quiet,
                     encoding, ...) {

    if (is.null(input_file))
      stop("Pass one file to render_site.",
           " wflow_build is the preferred function to use.")

    input_full <- absolute(input_file)
    input_base_wo_ext <- tools::file_path_sans_ext(basename(input_full))
    p <- wflow_paths(project = dirname(input_full))
    # wflow_yml <- file.path(p$root, "workflowr.yml")
    # wflow_config <- yaml::yaml.load_file(wflow_yml)

    tmp_rmd <- stringr::str_replace(input_file, "\\.[Rr]md$", "-wflow.Rmd")
    fig_tmp <- file.path(p$analysis, "figure", basename(tmp_rmd))

    lines_in <- readLines(input_file)
    lines_out <- lines_in
    # Find end of YAML header. Must start with "---" and can end with either
    # "---" or "...". The first regex below searches for a line that starts with
    # 3 dashes and the second regex searches for a line that starts with 3
    # periods.
    lines_delimiters <- sort(c(stringr::str_which(lines_in, "^-{3}"),
                               stringr::str_which(lines_in, "^\\.{3}")))
    if (length(lines_delimiters) < 2)
      stop("This file does not have a valid YAML header.", call. = FALSE)
    y <- lines_delimiters[2]

    # Only include some checks if the file contains at least one code chunk
    has_code <- any(stringr::str_detect(lines_in, "^```\\{.+\\}$"))

    # Add the date it was last updated
    lines_last_updated <- c("",
                            sprintf("**Last updated:** %s", Sys.Date()),
                            "")
    lines_out <- c(lines_out[1:y],
                   lines_last_updated,
                   lines_out[(y + 1):length(lines_out)])
    y <- y + length(lines_last_updated)


    if (has_code) {
      # Add the code version
      code_version <- extract_commit(p$root, num = 1)$sha1
      code_version <- stringr::str_sub(code_version, 1, 7)
      lines_code_version <- c("",
                              sprintf("**Code version:** %s", code_version),
                              "")
      lines_out <- c(lines_out[1:y],
                     lines_code_version,
                     lines_out[(y + 1):length(lines_out)])
      y <- y + length(lines_code_version)

      # Add the knitr chunk options
      cache_path <- paste0(input_base_wo_ext, "_cache")
      lines_opts_chunk <- c("",
                            "```{r knitr-opts-chunk-inserted-by-workflowr, include=FALSE}",
                            "knitr::opts_chunk$set(",
                            "  comment = NA,",
                            "  fig.align = \"center\",",
                            sprintf("  cache.path = \"%s/\",", cache_path),
                            "  fig.path = paste0(\"figure/\", knitr::current_input(), \"/\")",
                            ")",
                            "```",
                            "")
      lines_out <- c(lines_out[1:y],
                     lines_opts_chunk,
                     lines_out[(y + 1):length(lines_out)])
      y <- y + length(lines_opts_chunk)

      # Add session information at the end
      sessioninfo <- c("",
                       "## Session information",
                       "",
                       "```{r session-info-chunk-inserted-by-workflowr}",
                       "sessionInfo()",
                       "```",
                       "")
      lines_out <- c(lines_out, sessioninfo)
    }

    writeLines(lines_out, con = tmp_rmd)
    on.exit(unlink(tmp_rmd), add = TRUE)

    # For an R Markdown website, the output_options self_contained and lib_dir
    # must be set. Force them here instead of temporarily editing the _site.yml
    # file.
    #
    # However, only do this if the output is HTML (which is the default if
    # nothing is specified). The output can be non-HTML if the output section in
    # _site.yml lists a non-html format first. Lower precendence is if there is
    # nothing in _site.yml, it could be listed in the YAML header. There is no
    # need to worry about what is potentially passed to render_site because this
    # gets ignored.
    #
    # The other complication is that the output format can be specified as a
    # character vector if it has no options, or a list if it has options. The
    # conditional statements below are complicated in order to handle these
    # possibilities.
    is_html <- TRUE
    # First read the settings in _site.yml
    site_config <- yaml::yaml.load_file(file.path(p$analysis, "_site.yml"))
    if("output" %in% names(site_config)) {
      # If it has an output section, check it (can't do site_config$output b/c
      # that can expand to output_dir)
      if ((class(site_config$output) == "list" &&
           names(site_config$output)[1] != "html_document") ||
          (class(site_config$output) == "character" &&
           site_config$output[1] != "html_document")) {
        # The format is probably the name, but it can be the value if it is the
        # only one listed and doesn't specify "default".
        is_html <- FALSE
      }
      } else {
        # Check the first listed output format in the YAML header
        header <- rmarkdown::yaml_front_matter(input_file)
        if (!is.null(header$output) && !is.na(header$output) &&
            (class(header$output) == "list" &&
             names(header$output)[1] != "html_document") ||
            (class(header$output) == "character" &&
             header$output[1] != "html_document")) {
          is_html <- FALSE
        }
    }

    if (is_html) {
      output_options <- list(self_contained = FALSE,
                             lib_dir = "site_libs")
    } else {
      output_options <- NULL
    }

    suppressMessages(
      tmp_output <- rmarkdown::render(tmp_rmd,
                                      output_format = output_format,
                                      output_options = output_options,
                                      knit_root_dir = NULL,
                                      envir = envir,
                                      quiet = quiet,
                                      encoding = encoding)
    )

    move_safe <- function(from, to, overwrite = TRUE,
                          recursive = dir.exists(to)) {
      file.copy(from, to, overwrite = overwrite, recursive = recursive)
      unlink(from, recursive = TRUE)
    }
    # Remove -wflow from output filename
    output_file <- stringr::str_replace(tmp_output, "-wflow\\.", "\\.")
    move_safe(tmp_output, output_file)

    # Move the files if the output is a website
    if (is_html) {
      move_safe(output_file, p$docs)
      output_file <- file.path(p$docs, basename(output_file))
      # Move site libraries
      move_safe(file.path(p$analysis, "site_libs"), p$docs)
      # Move figures
      if (dir.exists(fig_tmp)) {
        fig_docs <- file.path(p$docs, "figure")
        dir.create(fig_docs, showWarnings = FALSE, recursive = FALSE)
        fig_analysis <- file.path(p$analysis, "figure", basename(input_file))
        file.rename(fig_tmp, fig_analysis)
        move_safe(fig_analysis, fig_docs)
        # Fix path to figures
        html_in <- readLines(output_file)
        html_out <- stringr::str_replace(html_in,
                                         file.path("figure", basename(tmp_rmd)),
                                         file.path("figure", basename(input_file)))
        writeLines(html_out, con = output_file)
      }
    }
    file.remove(tmp_rmd)
    if (!quiet) {
      message("\nOutput created: ", output_file)
    }
  }

  # return site generator
  list(
    name = "not implemented",
    output_dir = "not implemented",
    render = render,
    clean = function() stop("Not implemented. Use wflow_remove()", call. = FALSE)
  )
}
