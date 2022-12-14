#' Convert to a workflowr HTML document
#'
#' Workflowr custom format for converting from R Markdown to an HTML document.
#' \code{wflow_html} has two distinct functionalities: 1) configure the
#' formatting of the HTML by extending \code{\link[rmarkdown]{html_document}}
#' (see the
#' \href{https://bookdown.org/yihui/rmarkdown/html-document.html}{RStudio
#' documentation} for the available options), and 2) configure the workflowr
#' reproducibility features (typically specified in a file named
#' \code{_workflowr.yml}). \code{wflow_html} is intended to be used to generate
#' webpages for a workflowr website, but it can also be used outside a workflowr
#' project to implement reproducibility features for single R Markdown documents.
#'
#' @section HTML formatting:
#'
#' \code{wflow_html} extends
#' \code{\link[rmarkdown]{html_document}}. To set default formatting options to
#' be shared across all of your HTML files, set them in the file
#' \code{analysis/_site.yml}. This special file can also be used to configure
#' other aspects of the website like the navigation bar (for more details see
#' the documentation on
#' \href{https://bookdown.org/yihui/rmarkdown/rmarkdown-site.html}{R Markdown
#' websites}). For example, to use the theme "cosmo" and add a table of contents
#' to every webpage, you would add the following to \code{analysis/_site.yml}:
#'
#' \preformatted{
#' output:
#'   workflowr::wflow_html:
#'     toc: true
#'     theme: cosmo
#' }
#'
#' Formatting options can also be set for a specific file, which will override
#' the default options set in \code{analysis/_site.yml}. For example, to remove
#' the table of contents from one specific file, you would add the following to
#' the YAML header of that file:
#'
#' \preformatted{
#' output:
#'   workflowr::wflow_html:
#'     toc: false
#' }
#'
#' However, this will preserve any of the other shared options (e.g. the theme
#' in the above example). If you are not overriding any of the shared options,
#' it is not necessary to specify \code{wflow_html} in the YAML header of your
#' workflowr R Markdown files.
#'
#' @section Reproducibility features:
#'
#' \code{wflow_html} also implements the workflowr reproducibility features. For
#' example, it automatically sets a seed with \code{\link{set.seed}}; inserts
#' the current code version (i.e. Git commit ID); runs \code{\link{sessionInfo}}
#' at the end of the document; and inserts links to past versions of the file
#' and figures.
#'
#' These reproducibility options are not passed directly as arguments to
#' \code{wflow_html}. Instead these options are specified in
#' \code{_workflowr.yml} or in the YAML header of an R Markdown file (using the
#' field \code{workflowr:}). These options (along with their default values) are
#' as follows:
#'
#' \describe{
#'   \item{knit_root_dir}{The directory where code inside an R Markdown file is
#'   executed; this ultimately sets argument \code{knit_root_dir} in
#'   \code{\link[rmarkdown]{render}}. By default, \code{\link{wflow_start}} sets
#'   \code{knit_root_dir} in the file \code{_workflowr.yml} to be the path
#'   \code{"."}. This path is a
#'   \href{https://swcarpentry.github.io/shell-novice/reference.html#relative-path}{relative
#'   path} from the location of \code{_workflowr.yml} to the directory for the
#'   code to be executed. The path \code{"."} is shorthand for "current working
#'   directory", and thus code is executed in the root of the workflowr project.
#'   You can change this to be a relative path to any subdirectory of your
#'   project. Also, if you were to delete this line from \code{_workflowr.yml},
#'   then this would cause the code to be executed from the same directory in
#'   which the R Markdown files are located (i.e. \code{analysis/} in the
#'   default workflowr setup).
#'
#'   It is also possible (though in general not recommended) to configure the
#'   \code{knit_root_dir} to apply to only one of the R Markdown files by
#'   specifying it in the YAML header of that particular file. In this case, the
#'   supplied path is interpreted as relative to the R Markdown file itself.
#'   Thus \code{knit_root_dir: "../data"} would execute the code in the
#'   subdirectory \code{data/}.}
#'
#'   \item{seed}{The \code{seed} argument in the call to \code{\link{set.seed}},
#'   which is added to the beginning of an R Markdown file. In
#'   \code{\link{wflow_start}}, this is set to the date using the format
#'   \code{YYYYMMDD}. If no seed is specified, the default is \code{12345}.}
#'
#'   \item{sessioninfo}{The function that is run to record the session
#'   information. The default is \code{"sessionInfo()"}.}
#'
#'   \item{github}{The URL of the remote repository for creating links to past
#'   results. If unspecified, the URL is guessed from the "git remote" settings
#'   (see \code{\link{wflow_git_remote}}). Specifying this setting inside
#'   \code{_workflowr.yml} is especially helpful if multiple users are
#'   collaborating on a project since it ensures that everyone generates the
#'   same URLs.}
#'
#'   \item{suppress_report}{By default a workflowr report is inserted at the top
#'   of every HTML file containing useful summaries of the reproducibility
#'   features and links to past versions of the analysis. To suppress this
#'   report, set \code{suppress_report} to \code{TRUE}}.
#' }
#'
#' In the default workflowr setup, the file \code{_workflowr.yml} is located in
#' the root of the project. For most users it is best to leave it there, but if
#' you are interested in experimenting with the directory layout, the
#' \code{_workflowr.yml} file can be located in the same directory as the R
#' Markdown files or in any directory upstream of that directory.
#'
#' Here is an example of a customized \code{_workflowr.yml} file:
#'
#' \preformatted{
#' # Execute code in project directory
#' knit_root_dir: "."
#' # Set a custom seed
#' seed: 4815162342
#' # Use sessioninfo package to generate the session information.
#' sessioninfo: "sessioninfo::session_info()"
#' # Use this URL when inserting links to past results.
#' github: https://github.com/repoowner/mainrepo
#' }
#'
#' And here is an example of a YAML header inside an R Markdown file with the
#' same exact custom settings as above:
#'
#' \preformatted{
#' ---
#' title: "About"
#' output:
#'   workflowr::wflow_html:
#'     toc: false
#' workflowr:
#'   knit_root_dir: ".."
#'   seed: 4815162342
#'   sessioninfo: "sessioninfo::session_info()"
#'   github: https://github.com/repoowner/mainrepo
#' ---
#' }
#'
#' Note that the path passed to \code{knit_root_dir} changed to \code{".."}
#' because it is relative to the R Markdown file instead of
#' \code{_workflowr.yml}. Both have the effect of having the code executed in
#' the root of the workflowr project.
#'
#' @param ... Arguments passed to \code{\link[rmarkdown]{html_document}}.
#'
#' @return An \code{\link[rmarkdown]{output_format}} object to pass to
#' \code{\link[rmarkdown]{render}}.
#'
#' @seealso \code{\link{wflow_pre_knit}}, \code{\link{wflow_post_knit}},
#'          \code{\link{wflow_pre_processor}}
#'
#' @import rmarkdown
#'
#' @export
#'
wflow_html <- function(...) {

  cache_hook_final <- get_cache_hook()

  knitr <- rmarkdown::knitr_options(opts_chunk = list(comment = NA,
                                                      fig.align = "center",
                                                      tidy = FALSE),
                                    knit_hooks = list(plot = plot_hook,
                                                      chunk = cache_hook_final),
                                    opts_hooks = list(fig.path = hook_fig_path))

  # Have to explicitly pass on keep_md to output_format()
  opts <- list(...)
  if (!is.null(opts$keep_md)) keep_md <- opts$keep_md else keep_md <- FALSE

  o <- rmarkdown::output_format(knitr = knitr,
                                pandoc = pandoc_options(to = "html"),
                                keep_md = keep_md,
                                pre_knit = wflow_pre_knit,
                                post_knit = wflow_post_knit,
                                pre_processor = wflow_pre_processor,
                                base_format = rmarkdown::html_document(...))
  return(o)
}

# knitr options ----------------------------------------------------------------

# Save the figures in "figure/<basename-of-Rmd-file>/"
# https://yihui.name/knitr/hooks/#option-hooks
hook_fig_path <- function(options) {
  # Record the original figure path. If it was set by the user, a warning will
  # be inserted into the document by the knit hook `plot_hook` to notify that
  # the setting was ignored.
  options$fig.path.orig <- options$fig.path

  input <- knitr::current_input()
  options$fig.path <- create_figure_path(input)
  # Requires trailing slash
  options$fig.path <- paste0(options$fig.path, .Platform$file.sep)
  return(options)
}

# This knit hook inserts a table of previous versions of the figure
plot_hook <- function(x, options) {

  # Exit early if there is no Git repository
  if (!git2r::in_repository(".")) {
    return(wflow_hook_plot_md(x, options))
  }

  r <- git2r::repository(".", discover = TRUE)

  input <- file.path(getwd(), x)

  # Need to refactor obtaining workflowr options
  github = get_host_from_remote(getwd())
  output_dir <- get_output_dir(directory = getwd())
  if (!is.null(output_dir)) {
    input <- file.path(output_dir, x)
  }

  fig_versions <- get_versions_fig(fig = input, r = r, github = github)

  # Exit early if no previous versions of the figure are available
  if (fig_versions == "") {
    return(wflow_hook_plot_md(x, options))
  }

  return(paste(c(wflow_hook_plot_md(x, options), fig_versions),
               collapse = "\n"))
}

# Inserts Bootstrap warning(s) into the HTML file if any of the following
# issues are detected:
#
# * User set custom fig.path which gets ignored by workflowr
# * Python chunk creates a plot, but using old version of reticulate (< 1.15).
#   Older versions of reticulate used an absolute path to the figure file, which
#   caused the images to not be rendered in a non-standalone document.
wflow_hook_plot_md <- function(x, options) {

  warnings_to_add <- character()

  # Check fig.path
  expected <- paste0(tools::file_path_sans_ext(knitr::current_input()),
                     "_files", .Platform$file.sep, "figure-html",
                     .Platform$file.sep)
  if (options$fig.path.orig != expected) {
    warnings_to_add <- c(warnings_to_add,
                         "<strong>Warning!</strong> The custom <code>fig.path</code> you set was ignored by workflowr.")
  }

  if (length(warnings_to_add) == 0) {
    return(knitr::hook_plot_md(x, options))
  }

  # Add alert class
  warnings_to_add <- paste("<div class=\"alert alert-warning\">",
                           warnings_to_add,
                           "</div>")
  warnings_to_add <- paste(warnings_to_add, collapse = "\n")
  return(glue::glue("{knitr::hook_plot_md(x, options)}
                      {warnings_to_add}"))
}

# This knit hook warns if a chunk has cache=TRUE, autodep=FALSE, dependson=NULL
cache_hook <- function(x, options) {

  if (options$cache && is.null(options$dependson) && !options$autodep) {
    x <- glue::glue("{x}
                      <div class=\"alert alert-warning\">
                      <p><strong>Warning:</strong>
                      The above code chunk cached its results, but it won't be
                      re-run if previous chunks it depends on are updated. If
                      you need to use caching, it is highly recommended to
                      also set <code>knitr::opts_chunk$set(autodep =
                      TRUE)</code> at the top of the file (in a chunk that is
                      not cached). Alternatively, you can customize the option
                      <code>dependson</code> for each individual chunk that is
                      cached. Using either <code>autodep</code> or
                      <code>dependson</code> will remove this warning. See the
                      <a href=\"https://yihui.name/knitr/options/#cache\"
                      >knitr cache options</a> for more details.
                      </p>
                      </div>")
  }
  return(x)
}

# Access the default chunk hook function from knitr because it isn't exported.
knitr_hook_chunk <- function() {

  knitr::render_markdown()
  f <- knitr::knit_hooks$get("chunk")
  knitr::knit_hooks$restore()

  return(f)
}

# First run the chunk through knitr's default markdown chunk function
get_cache_hook <- function() {

  default_hook_chunk <- knitr_hook_chunk()
  wflow_hook_chunk <- cache_hook

  result <- function(x, options) {

    x <- default_hook_chunk(x, options)
    wflow_hook_chunk(x, options)
  }
  return(result)
}

# pre_knit function ------------------------------------------------------------

#' pre_knit function for workflowr
#'
#' This is the \code{pre_knit} function that \code{\link{wflow_html}} passes to
#' the function \code{\link[rmarkdown]{output_format}} from the package
#' \link{rmarkdown}. For advanced usage only.
#'
#' If you'd like to insert the workflowr reproducibility report into other R
#' Markdown output formats such as \code{blogdown::html_page}, you can use
#' \code{wflow_pre_knit}.
#'
#' @param input Name of R Markdown file
#' @param ... currently ignored
#'
#' @seealso \code{\link{wflow_html}}, \code{\link{wflow_post_knit}},
#'          \code{\link{wflow_pre_processor}}
#'
#' @export
#' @keywords internal
wflow_pre_knit <- function(input, ...) {
  # This function copies the R Markdown file to a temporary directory and then
  # modifies it.

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

  wflow_opts <- wflow_options(input)

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
    sessioninfo <- glue::glue('
      <br>
      <p>
      <button type="button" class="btn btn-default btn-workflowr btn-workflowr-sessioninfo"
        data-toggle="collapse" data-target="#workflowr-sessioninfo"
        style = "display: block;">
        <span class="glyphicon glyphicon-wrench" aria-hidden="true"></span>
        Session information
      </button>
      </p>

      <div id="workflowr-sessioninfo" class="collapse">
      ```{{r session-info-chunk-inserted-by-workflowr}}
      {wflow_opts$sessioninfo}
      ```
      </div>
      ')
    # If there is a bibliography, make sure it appears before the session
    # information
    header <- rmarkdown::yaml_front_matter(input)
    if (!is.null(header$bibliography)) {
      sessioninfo <- add_bibliography(sessioninfo, lines_in)
    }
  } else {
    sessioninfo <- ""
  }

  lines_out <- c(header_lines,
                 "",
                 report,
                 "",
                 seed_chunk,
                 "",
                 lines_in[(header_end + 1):length(lines_in)],
                 "",
                 sessioninfo)

  writeLines(lines_out, tmpfile)
}

# Add the bibliography prior to the session information, but only if they
# haven't manually inserted the bibliography already.
#
# sessioninfo - character vector with session information lines to insert at end
# of R Markdown file
#
# lines - character vector of the lines of current R Markdown file
#
# Prepends <div id="refs"></div> if this string is not already present in the
# documents.
add_bibliography <- function(sessioninfo, lines) {
  stopifnot(is.character(sessioninfo), is.character(lines))
  if (!any(stringr::str_detect(lines, "<div id=[\'\"]refs[\'\"]>"))) {
    sessioninfo <- c("", "<div id=\"refs\"></div>", "", sessioninfo)
  }
  return(sessioninfo)
}

# post_knit function -----------------------------------------------------------

#' post_knit function for workflowr
#'
#' This is the \code{post_knit} function that \code{\link{wflow_html}} passes to
#' the function \code{\link[rmarkdown]{output_format}} from the package
#' \link{rmarkdown}. For advanced usage only.
#'
#' If you'd like to combine workflowr with another R Markdown output format, you
#' may need to use \code{wflow_post_knit}. This function fixes the path to the R
#' Markdown file (which is manipulated by \code{\link{wflow_pre_knit}}).
#'
#' @param metadata The metadata specified in the YAML header of the R Markdown
#'   file
#' @param input_file Name of R Markdown file
#' @param ... arguments passed to the \code{post_knit} function of
#'   \code{rmarkdown::\link[rmarkdown]{html_document}}
#'
#' @inheritParams rmarkdown::render
#'
#' @seealso \code{\link{wflow_html}}, \code{\link{wflow_pre_knit}},
#'          \code{\link{wflow_pre_processor}}
#'
#' @export
#' @keywords internal
wflow_post_knit <- function(metadata, input_file, runtime, encoding, ...) {
  # This function adds the navigation bar for websites defined in either
  # _navbar.html or _site.yml. Below I just fix the path to the input file that
  # I had changed for pre_knit and then execute the post_knit from
  # rmarkdown::html_document.

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

#' pre_processor function for workflowr
#'
#' This is the \code{pre_processor} function that \code{\link{wflow_html}}
#' passes to the function \code{\link[rmarkdown]{output_format}} from the
#' package \link{rmarkdown}. For advanced usage only.
#'
#' If you'd like to combine workflowr with another R Markdown output format, you
#' may need to use \code{wflow_pre_processor}. This function only has minor
#' effects on the style of the resulting HTML page, and is not essential for
#' using workflowr.
#'
#' @param input_file Name of Markdown file created by
#'   \code{knitr::\link[knitr]{knit}} to be passed to
#'   \href{https://pandoc.org/}{pandoc}
#' @param files_dir Directory for saving intermediate files
#'
#' @inheritParams rmarkdown::render
#' @inheritParams wflow_post_knit
#'
#' @seealso \code{\link{wflow_html}}, \code{\link{wflow_pre_knit}},
#'          \code{\link{wflow_post_knit}}
#'
#' @export
#' @keywords internal
wflow_pre_processor <- function(metadata, input_file, runtime, knit_meta,
                                files_dir, output_dir) {
  # Pass additional arguments to Pandoc. I use this to add a custom header
  # (--include-in-header) and footer (--include-after-body). The template text
  # for these are in the list `includes` defined in R/infrastructure.R.

  # header
  fname_header <- tempfile("header", fileext = ".html")
  writeLines(includes$header, con = fname_header)

  # footer
  fname_footer <- tempfile("footer", fileext = ".html")
  writeLines(includes$footer, con = fname_footer)

  # Pandoc args
  args <- c("--include-in-header", fname_header,
            "--include-after-body", fname_footer)

  # Add pagetitle if missing title to avoid pandoc2 error
  if (rmarkdown::pandoc_available("2.0")) {
     args <- c(args, add_pagetitle(metadata, input_file))
  }

  return(args)
}

# Add a pagetitle (if needed) to avoid pandoc2 warning about missing title
add_pagetitle <- function(metadata, input_file) {
  # Only add pagetitle if all the following conditions are met

  # No title
  if (!is.null(metadata$title)) return(character(0))

  # No pagetitle
  if (!is.null(metadata$pagetitle)) return(character(0))

  # No title/pagetitle defined with pandoc_args
  if (is.list(metadata$output)) {
      pandoc_args <- metadata$output$`workflowr::wflow_html`$pandoc_args
  } else {
    pandoc_args <- NULL
  }
  if (!is.null(pandoc_args) && any(stringr::str_detect(pandoc_args, "title")))
    return(character(0))

  pagetitle <- input_file
  return(c("--metadata", paste0("pagetitle=", pagetitle)))
}
