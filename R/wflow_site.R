#' Custom site generator for workflowr websites
#'
#' \code{wflow_site} is a
#' \href{http://rmarkdown.rstudio.com/rmarkdown_site_generators.html}{custom
#' site generator} to be used in combination with the R Markdown output format
#' \code{\link{wflow_html}}.
#'
#' Do not call the function \code{wflow_site} directly. Instead insert the line
#' below directly into the YAML header of the file \code{index.Rmd}:
#'
#' \preformatted{
#' ---
#' title: "Home"
#' site: workflowr::wflow_site
#' output:
#'   workflowr::wflow_html:
#'     toc: false
#' ---
#' }
#'
#' Then you can build the website by running \code{\link[rmarkdown]{render_site}}
#' in the R console or clicking the Knit button in RStudio.
#'
#' If you receive an error when using the RStudio Knit button (the error is
#' about an unused argument), make sure the Knit Directory is set to Document
#' Directory (you can set this with the dropdown menu next to the Knit button).
#'
#' @param input character. The name of the website directory or a specific R
#'   Markdown file in the website directory.
#' @param encoding character. The
#'   \href{https://en.wikipedia.org/wiki/Character_encoding}{character encoding}
#'   to use to read the file.
#' @param ... Placeholder for potential future use.
#'
#' @seealso \code{\link{wflow_html}}, \code{\link[rmarkdown]{render_site}}
#'
#' @import rmarkdown
#' @export
wflow_site <- function(input, encoding = getOption("encoding"), ...) {

  # Get output directory if it exists
  output_dir <- get_output_dir(directory = input)

  render <- function(input_file,
                     output_format,
                     envir,
                     quiet,
                     encoding, ...) {

    # input is defined in the enclosing environment, i.e. wflow_site
    input <- absolute(input)

    if (is.null(input_file)) {
      files <- list.files(input, pattern = "^[^_].*\\.[Rr]md$",
                          full.names = TRUE)
    } else {
      files <- input_file
    }

    # For an R Markdown website, the output_options self_contained and lib_dir
    # must be set. Force them here instead of temporarily editing the _site.yml
    # file.
    # To improve: only do this for HTML output:
    # https://github.com/rstudio/rmarkdown/pull/1177
    output_options <- list(self_contained = FALSE,
                           lib_dir = "site_libs")

    for (f in files) {
      suppressMessages(
        output_file <- rmarkdown::render(f,
                                         output_format = output_format,
                                         output_options = output_options,
                                         knit_root_dir = NULL,
                                         envir = envir,
                                         quiet = quiet,
                                         encoding = encoding)
      )

      # output_dir is defined in the enclosing environment (i.e. render is
      # defined inside of wflow_html)

      if (output_dir != input) {
        # Move HTML file
        fs::file_copy(output_file, output_dir, overwrite = TRUE)
        unlink(output_file)
        output_file <- file.path(output_dir, basename(output_file))

        # Move figures
        fig_dir <- create_figure_path(f)
        fig_dir <- file.path(input, fig_dir)

        if (fs::dir_exists(fig_dir)) {
          fig_output_dir <- file.path(output_dir, "figure")
          fs::dir_create(fig_output_dir)
          file.copy(fig_dir, fig_output_dir, recursive = TRUE)
          unlink(fig_dir, recursive = TRUE)
        }

        # Copy CSS/Javascript files
        files_css <- list.files(path = input, pattern = "css$", full.names = TRUE)
        fs::file_copy(files_css, output_dir, overwrite = TRUE)
        files_js <- list.files(path = input, pattern = "js$", full.names = TRUE)
        fs::file_copy(files_js, output_dir, overwrite = TRUE)
      }
    }

    # Clean up source directory
    if (output_dir != input) {
      # Move site libraries
      site_libs <- file.path(input, "site_libs")
      file.copy(site_libs, output_dir, recursive = TRUE)
      unlink(site_libs, recursive = TRUE)
      # Remove figure directory
      unlink(file.path(input, "figure"), recursive = TRUE)
    }

    # Open in RStudio Viewer
    if (!quiet) {
      message("\nOutput created: ", output_file)
    }
  }

  # return site generator
  list(
    name = "not implemented",
    output_dir = output_dir,
    render = render,
    clean = function() stop("Not implemented", call. = FALSE)
  )
}
