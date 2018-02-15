#' Convert workflowr analysis to HTML document
#'
#' @param ...
#'
#' @return
#'
#' @import rmarkdown
#' @export
wflow_html <- function(...) {


  # knitr options --------------------------------------------------------------

  # https://yihui.name/knitr/hooks/#option-hooks
  hook_fig_path <- function(options) {
    options$fig.path <- file.path("figure", knitr::current_input(), "")
    return(options)
  }

  knitr <- rmarkdown::knitr_options(opts_chunk = list(comment = NA,
                                                      fig.align = "center",
                                                      tidy = FALSE),
                                    opts_hooks = list(fig.path = hook_fig_path))

  # pre_knit function ----------------------------------------------------------

  pre_knit <- function(input, ...) {

    # Access parent environment. Have to go up 2 frames because of the function
    # that combines pre_knit function from the current and base output_formats.
    #
    # Inspired by rmarkdowntown by Romain François
    # https://github.com/romainfrancois/rmarkdowntown/blob/deef97a5cd6f0592318ecc6e78c6edd7612eb449/R/html_document2.R#L12
    frames <- sys.frames()
    e <- frames[[length(frames) - 2]]

    # Set knit_root_dir to the location of the original file
    e$knit_root_dir <- dirname(absolute(input))

    lines_in <- readLines(input)
    tmpfile <- file.path(tempdir(), basename(input))
    lines_out <- lines_in

    # Add session information at the end
    sessioninfo <- c("",
                     "## Session information",
                     "",
                     "```{r session-info-chunk-inserted-by-workflowr}",
                     "sessionInfo()",
                     "```",
                     "")
    lines_out <- c(lines_out, sessioninfo)

    writeLines(lines_out, tmpfile)
    e$knit_input <- tmpfile
  }

  # post_knit function ---------------------------------------------------------

  post_knit <- function(metadata, input_file, runtime, encoding, ...) {

    # Change the input_file back to its original so that the post_knit defined
    # in rmarkdown::html_document() can find the navbar defined in _site.yml.
    input_file_original <- file.path(getwd(), basename(input_file))
    # I tried to find a better solution than directly calling it myself (since
    # it is run afterwards anyways since html_document() is the base format),
    # but nothing I tried worked.
    rmarkdown::html_document()$post_knit(metadata, input_file_original,
                                         runtime, encoding, ...)
  }

  # Return ---------------------------------------------------------------------

  o <- rmarkdown::output_format(knitr = knitr,
                                pandoc = pandoc_options(to = "html"),
                                pre_knit = pre_knit,
                                post_knit = post_knit,
                                base_format = rmarkdown::html_document(...))
  return(o)
}
