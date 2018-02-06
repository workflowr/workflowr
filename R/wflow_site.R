
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
    p <- wflow_paths(project = dirname(input_full))
    # wflow_yml <- file.path(p$root, "workflowr.yml")
    # wflow_config <- yaml::yaml.load_file(wflow_yml)

    tmp_rmd <- stringr::str_replace(input_file, "\\.[Rr]md$", "-wflow.Rmd")
    tmp_html <- stringr::str_replace(tmp_rmd, "-wflow\\.Rmd$", ".html")

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

    # Add the date it was last updated
    lines_last_updated <- c("",
                            sprintf("**Last updated:** %s", Sys.Date()),
                            "")
    lines_out <- c(lines_out[1:y],
                   lines_last_updated,
                   lines_out[(y + 1):length(lines_out)])
    y <- y + length(lines_last_updated)

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

    # Add session information at the end
    sessioninfo <- c("",
                     "## Session information",
                     "",
                     "```{r session-info-chunk-inserted-by-workflowr}",
                     "sessionInfo()",
                     "```",
                     "")
    lines_out <- c(lines_out, sessioninfo)

    writeLines(lines_out, con = tmp_rmd)
    # output_file needs to be relative to the knit directory
    tmp_html <- relative(tmp_html, start = p$analysis)
    suppressMessages(
      rmarkdown::render(tmp_rmd,
                        output_file = relative(tmp_html, start = p$analysis))
    )
    move_safe <- function(from, to, overwrite = TRUE) {
      file.copy(from, to, overwrite = overwrite)
      file.remove(from)
    }
    move_safe(tmp_html, p$docs)
    file.remove(tmp_rmd)
  }
  # return site generator
  list(
    name = "not implemented",
    output_dir = "not implemented",
    render = render,
    clean = function() stop("Not implemented. Use wflow_remove()", call. = FALSE)
  )
}

#' @export
wflow_document <- function(...) {
  x <- rmarkdown::html_document(...)
  x$knitr$opts_knit$root.dir <- ".."
  x$knitr$opts_chunk$comment <- NA
  x$knitr$opts_chunk$fig.align <- "center"
  x$knitr$opts_chunk$tidy <- FALSE
  x
}
