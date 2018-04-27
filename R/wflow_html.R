#' Convert to a workflowr HTML document
#'
#' Workflowr custom format for converting from R Markdown to an HTML
#' document. See \code{\link[rmarkdown]{render}}
#' \code{\link[rmarkdown]{render_site}} for an explanation of how this
#' conversion format can be specified inside the \code{_site.yml}
#' configuration file or an R Markdown file. This is intended to be
#' used to generate webpages for a workflowr website, but it can also
#' be used outside a workflowr project to generate webpages from R
#' Markdown documents.
#'
#' The output format \code{wflow_html} provides a number of features
#' including: automatically setting a seed with \code{\link{set.seed}};
#' inserting the Git commit id; providing \code{\link{sessionInfo}} at
#' the end of the document; and inserting links to past versions of the
#' file and figures.
#'
#' \code{wflow_html} extends
#' \code{\link[rmarkdown]{html_document}}. To customize how webpages
#' are rendered from R Markdown files, you can specify some of these
#' settings directly in the call to \code{wflow_html}, or you can edit
#' \code{_site.yml}, or you can specific the settings in an R Markdown
#' file. For more information, see \code{\link[rmarkdown]{render}},
#' \code{\link[rmarkdown]{render_site}},
#' \url{https://rmarkdown.rstudio.com/html_document_format.html} and
#' \url{https://rmarkdown.rstudio.com/rmarkdown_websites.html}.
#'
#' Additional settings specific to \code{wflow_html} (\emph{i.e.,}
#' settings not inherited from \code{\link[rmarkdown]{html_document}})
#' are specified in \code{_workflowr.yml} or in the R Markdown file.
#' These settings (along with their default values) are as follows:
#'
#' \describe{
#'   \item{knit_root_dir}{The directory where code inside an R
#'   Markdown file is executed; this ultimately sets argument
#'   \code{knit_root_dir} in \code{\link[rmarkdown]{render}}. By
#'   default, it is set to the location of the R Markdown file. To
#'   override this default, specify the setting in \code{_workflowr.yml}
#'   (see below for examples). If the specified path is a relative path,
#'   this path is relative to the location of the \code{_workflowr.yml}
#'   file or the R Markdown file. For example, if it is set to
#'   \code{"."} in \code{_workflowr.yml}, then the code is executed in
#'   the directory where the \code{_workflowr.yml} is located; if it is
#'   set to \code{"."} inside \code{myanalysis.Rmd}, then the code is
#'   excuted in the same directory as \code{myanalysis.Rmd}. For
#'   workflowr functions such as \code{\link{wflow_build}} which are run
#'   from the root directory of the workflowr project, this particular
#'   setting results in the code being executed from the project root.}
#'
#'   \item{seed}{The \code{seed} argument in the call to
#'   \code{\link{set.seed}}, which is added to beginning of an R
#'   Markdown file. In \code{\link{wflow_start}}, this is set to the
#'   date in format \code{YYYYMMDD}. If no seed is specified, the
#'   default is \code{12345}.}
#'
#'   \item{sessioninfo}{The function that is run to record the
#'   session information. The default is \code{sessionInfo()}.}
#'
#'   \item{github}{The URL of the GitHub repository for creating links
#'   to past results. If unspecified, the URL is guessed from the "git
#'   remote" settings (see \code{\link{wflow_git_remote}}). Specifying
#'   this setting inside \code{_workflowr.yml} is especially helpful if
#'   multiple users are collaborating on a project since it ensures that
#'   everyone generates the same URLs.}
#' }
#'
#' If you would like to adjust any of these settings inside the
#' \code{_workflowr.yml} configuration file, this file must be the
#' same directory as the R Markdown file, or an upstream directory.
#' 
#' Here is an example of a customized \code{_workflowr.yml} file:
#'
#' \preformatted{
#' # Execute code in project directory
#' knit_root_dir: "."
#' # Set a custom seed
#' seed: 4815162342
#' # Use devtools to generate the session information.
#' sessioninfo: "devtools::session_info()"
#' # Use this URL when inserting links to past results.
#' github: https://github.com/repoowner/mainrepo
#' }
#'
#' And here is an example of a YAML header inside an R Markdown file
#' with custom settings:
#'
#' \preformatted{
#' ---
#' title: "About"
#' output:
#'   workflowr::wflow_html:
#'     toc: false
#' workflowr:
#'   knit_root_dir: "."
#'   seed: 4815162342
#'   sessioninfo: "devtools::session_info()"
#'   github: https://github.com/repoowner/mainrepo
#' ---
#' }
#'
#' @param ... Arguments passed to \code{\link[rmarkdown]{html_document}}.
#'
#' @return An \code{\link[rmarkdown]{output_format}} object to pass to
#' \code{\link[rmarkdown]{render}}.
#'
#' @import rmarkdown
#' 
#' @export
#' 
wflow_html <- function(...) {

  # knitr options --------------------------------------------------------------

  # Save the figures in "figure/<basename-of-Rmd-file>/"
  # https://yihui.name/knitr/hooks/#option-hooks
  hook_fig_path <- function(options) {
    options$fig.path <- file.path("figure", knitr::current_input())
    # Requires trailing slash
    options$fig.path <- paste0(options$fig.path, .Platform$file.sep)
    return(options)
  }
  plot_hook <- function(x, options) {
    if (git2r::in_repository(".")) {
      r <- git2r::repository(".", discover = TRUE)

      input <- file.path(getwd(), x)

      # Need to refactor obtaining workflowr options
      github = get_github_from_remote(getwd())
      output_dir <- get_output_dir(directory = getwd())
      if (!is.null(output_dir)) {
        input <- file.path(output_dir, x)
      }

      fig_versions <- get_versions_fig(fig = input, r = r, github = github)

      if (fig_versions == "") {
        return(knitr::hook_plot_md(x, options))
      } else {
        paste(c(knitr::hook_plot_md(x, options),
                fig_versions),
              collapse = "\n")
      }
    } else {
      return(knitr::hook_plot_md(x, options))
    }
  }

  knitr <- rmarkdown::knitr_options(opts_chunk = list(comment = NA,
                                                      fig.align = "center",
                                                      tidy = FALSE),
                                    knit_hooks = list(plot = plot_hook),
                                    opts_hooks = list(fig.path = hook_fig_path))

  # pre_knit function ----------------------------------------------------------

  # This function copies the R Markdown file to a temporary directory and then
  # modifies it.
  pre_knit <- function(input, ...) {

    # Access parent environment. Have to go up 2 frames because of the function
    # that combines pre_knit function from the current and base output_formats.
    #
    # Inspired by rmarkdowntown by Romain François
    # https://github.com/romainfrancois/rmarkdowntown/blob/deef97a5cd6f0592318ecc6e78c6edd7612eb449/R/html_document2.R#L12
    frames <- sys.frames()
    e <- frames[[length(frames) - 2]]

    lines_in <- readLines(input)
    tmpfile <- file.path(tempdir(), basename(input))
    e$knit_input <- tmpfile

    # Default wflow options
    wflow_opts <- list(knit_root_dir = NULL,
                        seed = 12345,
                        github = get_github_from_remote(dirname(input)),
                        sessioninfo = "sessionInfo()")

    # Get options from a potential _workflowr.yml file
    wflow_root <- try(rprojroot::find_root(rprojroot::has_file("_workflowr.yml"),
                                            path = dirname(input)), silent = TRUE)
    if (class(wflow_root) != "try-error") {
      wflow_yml <- file.path(wflow_root, "_workflowr.yml")
      wflow_yml_opts <- yaml::yaml.load_file(wflow_yml)
      for (opt in names(wflow_yml_opts)) {
        wflow_opts[[opt]] <- wflow_yml_opts[[opt]]
      }
      # If knit_root_dir is a relative path, interpret it as relative to the
      # location of _workflowr.yml
      if (!is.null(wflow_opts$knit_root_dir)) {
        if (!R.utils::isAbsolutePath(wflow_opts$knit_root_dir)) {
          wflow_opts$knit_root_dir <- absolute(file.path(wflow_root,
                                                          wflow_opts$knit_root_dir))
        }
      }
    }

    # Get potential options from YAML header. These override the options
    # specified in _workflowr.yml.
    header <- rmarkdown::yaml_front_matter(input)
    header_opts <- header$workflowr
    for (opt in names(header_opts)) {
      wflow_opts[[opt]] <- header_opts[[opt]]
    }
    # If knit_root_dir was specified as a relative path in the YAML header,
    # interpret it as relative to the location of the file
    if (!is.null(wflow_opts$knit_root_dir)) {
      if (!R.utils::isAbsolutePath(wflow_opts$knit_root_dir)) {
        wflow_opts$knit_root_dir <- absolute(file.path(dirname(input),
                                                        wflow_opts$knit_root_dir))
      }
    }

    # If knit_root_dir hasn't been configured in _workflowr.yml or the YAML header,
    # set it to the location of the original file
    if (is.null(wflow_opts$knit_root_dir)) {
      wflow_opts$knit_root_dir <- dirname(absolute(input))
    }

    # Set the knit_root_dir option for rmarkdown::render. However, the user can
    # override the knit_root_dir option by passing it directly to render.
    if (is.null(e$knit_root_dir)) {
      e$knit_root_dir <- wflow_opts$knit_root_dir
    } else {
      wflow_opts$knit_root_dir <- e$knit_root_dir
    }

    # Find the end of the YAML header for inserting new lines
    header_delims <- stringr::str_which(lines_in, "^-{3}|^\\.{3}")
    if (length(header_delims) >= 2) {
      header_end <- header_delims[2]
      header_lines <- lines_in[seq(header_end)]
    } else {
      # A valid R Markdown file does not require a YAML header
      header_end <- 0
      header_lines <- NULL
    }

    # Get output directory if it exists
    output_dir <- get_output_dir(directory = dirname(input))

    has_code <- detect_code(input)

    report <- create_report(input, output_dir, has_code, wflow_opts)

    # Set seed at beginning
    if (has_code && is.numeric(wflow_opts$seed) && length(wflow_opts$seed) == 1) {
      seed_chunk <- c("",
                      "```{r seed-set-by-workflowr, echo = FALSE}",
                      sprintf("set.seed(%d)", wflow_opts$seed),
                      "```",
                      "")
    } else {
      seed_chunk <- ""
    }

    # Add session information at the end
    if (has_code && wflow_opts$sessioninfo != "") {
      sessioninfo <- c("",
                       "## Session information",
                       "",
                       "```{r session-info-chunk-inserted-by-workflowr}",
                       wflow_opts$sessioninfo,
                       "```",
                       "")
    } else {
      sessioninfo <- ""
    }

    lines_out <- c(header_lines,
                   "**Last updated:** `r Sys.Date()`",
                   report,
                   "---",
                   seed_chunk,
                   lines_in[(header_end + 1):length(lines_in)],
                   sessioninfo)

    writeLines(lines_out, tmpfile)
  }

  # post_knit function ---------------------------------------------------------

  # This function adds the navigation bar for websites defined in either
  # _navbar.html or _site.yml. Below I just fix the path to the input file that
  # I had changed for pre_knit and then execute the post_knit from
  # rmarkdown::html_document.
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

  # pre_processor function -----------------------------------------------------

  # Pass additional arguments to Pandoc. I use this to add a custom header
  # (--include-before-body) and footer (--include-after-body). The template text
  # for these are in the list `includes` defined in R/infrastructure.R.
  pre_processor <- function(metadata, input_file, runtime, knit_meta,
                            files_dir, output_dir) {
    # header
    fname_header <- tempfile("header", fileext = ".html")
    writeLines(includes$header, con = fname_header)

    # footer
    fname_footer <- tempfile("footer", fileext = ".html")
    wflow_version <- utils::packageVersion("workflowr")
    footer <- glue::glue(includes$footer)
    writeLines(footer, con = fname_footer)

    # Pandoc args
    args <- c("--include-before-body", fname_header,
              "--include-after-body", fname_footer)
    return(args)
  }

  # Return ---------------------------------------------------------------------

  o <- rmarkdown::output_format(knitr = knitr,
                                pandoc = pandoc_options(to = "html"),
                                pre_knit = pre_knit,
                                post_knit = post_knit,
                                pre_processor = pre_processor,
                                base_format = rmarkdown::html_document(...))
  return(o)
}
