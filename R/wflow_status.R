#' Report status of workflowr project.
#'
#' \code{wflow_status} reports the current status of the workflowr project.
#'
#' \code{wflow_status} reports the paths to the main directories of the
#' workflowr project:
#'
#' \itemize{
#'
#' \item \bold{working directory}: The current directory in the R console (i.e.
#' \code{getwd{}}).
#'
#' \item \bold{project root}: The directory that contains the RStudio .Rproj
#' file.
#'
#' \item \bold{analysis}: The directory that contains \code{_site.yml} and the R
#' Markdown files.
#'
#' \item \bold{docs}: The directory that contains the HTML files and figures.
#'
#' \item \bold{git}: The \code{.git} directory that contains the history of the
#' Git repository.
#'
#' }
#'
#' \code{wflow_status} also reports the status of files that require user
#' action:
#'
#' \itemize{
#'
#' \item \bold{outdated}: When an R Markdown file has been committed to the
#' repository without updating the previously published HTML file.
#'
#' \item \bold{staged}: When an R Markdown file has changes that have been added
#' to the index (e.g. with \code{git add}).
#'
#' \item \bold{unstaged}: When a tracked R Markdown file has changes in the
#' working directory.
#'
#' \item \bold{untracked}: When an R Markdown file has not been added or
#' committed to the Git repository.
#'
#' \item \bold{ignored}: When an R Markdown file has been ignored by Git
#' according to the patterns in the file \code{.gitignore}.
#'
#' }
#'
#' @param verbose logical (default: TRUE). Prints status to R console.
#' @param project character (default: ".") By default the function assumes the
#'   current working directory is within the project. If this is not true,
#'   you'll need to provide the path to the project directory.
#'
#' @return Invisibly returns an object of class \code{wflow_status}.
#'
#' @examples
#' \dontrun{
#'
#' wflow_status()
#' # Save the results
#' s <- wflow_status()
#' }
#' @export
wflow_status <- function(verbose = TRUE, project = ".") {
  if (!is.logical(verbose) | length(verbose) != 1)
    stop("verbose must be a one-element logical vector")
  if (!is.character(project) | length(project) != 1)
    stop("project must be a one element character vector")
  if (!dir.exists(project))
    stop("project does not exist.")

  # Create list to store output
  o <- list()
  class(o) <- "wflow_status"

  # Working directory
  o$wd <- getwd()

  # workflowr root
  project <- normalizePath(project)
  o$root <- try(rprojroot::find_rstudio_root_file(path = project),
                silent = TRUE)
  if (class(o$root) == "try-error")
    stop(wrap(
      "Unable to find RStudio Rproj file at the root of the workflowr project.
      Did you delete it?"),
      call. = FALSE)

  # Analysis directory with _site.yml
  top_level_files <- list.files(path = o$root, full.names = TRUE)
  subdirs <- top_level_files[dir.exists(top_level_files)]
  site_file <- list.files(path = subdirs, pattern = "_site.yml",
                          full.names = TRUE)
  if (length(site_file) == 0) {
    stop("Unable to find the file _site.yml in the analysis directory. Is this a workflowr project?", call. = FALSE)
  } else if (length(site_file) > 1) {
    stop("Found more than one _site.yml file. Only one subdirectory at the top level of the workflowr project can contain _site.yml.", call. = FALSE)
  } else {
    o$analysis <- dirname(site_file)
  }

  # docs/ directory
  output_dir <- yaml::yaml.load_file(site_file)$output_dir
  if (is.null(output_dir))
    stop("Unable to locate the website directory. Make sure to set the variable output_dir in the file _site.yml", call. = FALSE)
  o$docs <- normalizePath(file.path(o$analysis, output_dir), mustWork = FALSE)
  if (!dir.exists(o$docs)) {
    o$docs <- NA
    warning("Unable to locate docs directory. Run wflow_build() to create it.")
  }

  # Gather analysis files
  # (files that start with an underscore are ignored)
  files_all <- list.files(path = o$analysis, pattern = "^[^_]", full.names = TRUE)
  files_all_ext <- tools::file_ext(files_all)
  files_analysis <- files_all[files_all_ext %in% c("Rmd", "rmd")]
  o$files <- files_analysis

  # Git repository
  r <- try(git2r::repository(o$root, discover = TRUE), silent = TRUE)
  if (class(r) == "try-error") {
    o$git <- NA
  } else {
    o$git <- normalizePath(r@path) # remove trailing slash
  }

  # Obtain status of each file
  if (!is.na(o$git)) {
    s <- git2r::status(r, ignored = TRUE)
    o$git_status <- s
    # Convert from a list of lists of relative paths to a list of character
    # vectors of absolute paths
    s <- lapply(s, function(x) paste0(git2r::workdir(r), as.character(x)))
    # Determine status of each analysis file in the Git repository. Each status
    # is a logical vector.
    tracked <- files_analysis %in% setdiff(files_analysis,
                                           c(s$untracked, s$ignored))
    staged <- files_analysis %in% s$staged
    unstaged <- files_analysis %in% s$unstaged
    ignored <- files_analysis %in% s$ignored
    o$status <- data.frame(tracked, staged, unstaged, ignored,
                           row.names = files_analysis)
    if (dir.exists(o$docs)) {
      html <- to_html(files_analysis, outdir = o$docs)
      # Has the HTML file been built?
      built <- file.exists(html)
      # Has the HTML file been committed?
      committed <- paste0(git2r::workdir(r), get_committed_files(r))
      published <- html %in% committed
      # Is the committed HTML file up-to-date?
      files_outdated <- get_outdated_files(r, files_analysis[published],
                                           outdir = o$docs)
      up_to_date <- published & !(files_analysis %in% files_outdated)
      o$status <- cbind(o$status, built, published, up_to_date)
    }
  }

  if (verbose)
    print(o)

  return(invisible(o))
}

#' @export
print.wflow_status <- function(x, ...) {

  template <-
"workflowr project status report
-------------------------------

You have the following directories set:

working:      \"{{{wd}}}\"
project root: \"{{root}}\"
analysis:     \"{{analysis}}\"
docs:         \"{{docs}}\"
git:          \"{{git}}\"

You have {{n_analysis}} R Markdown files ({{n_published}} published).

{{#display_outdated}}
  * {{n_outdated}} outdated.

{{{f_outdated}}}

{{/display_outdated}}
{{#display_unpublished}}
  * {{n_unpublished}} unpublished.

{{{f_unpublished}}}

{{/display_unpublished}}
{{#display_staged}}
  * {{n_staged}} with staged changes.

{{{f_staged}}}

{{/display_staged}}
{{#display_unstaged}}
  * {{n_unstaged}} with unstaged changes.

{{{f_unstaged}}}

{{/display_unstaged}}
{{#display_untracked}}
  * {{n_untracked}} untracked by Git.

{{{f_untracked}}}

{{/display_untracked}}
{{#display_ignored}}
  * {{n_ignored}} ignored by Git.

{{{f_ignored}}}

{{/display_ignored}}

To publish a file as part of your site, use `wflow_publish()`. To commit a
file to the Git repository without yet publishing the HTML, use
`wflow_commit()`.
"

  published <- x$status$published
  outdated <- x$status$published & !x$status$up_to_date
  unpublished <- x$status$tracked & !x$status$published
  staged <- x$status$staged
  unstaged <- x$status$unstaged
  untracked <- !x$status$tracked
  ignored <-x$status$ignored

  variables <- list(
    wd = x$wd,
    root = x$root,
    analysis = x$analysis,
    docs = x$docs,
    git = x$git,
    n_analysis = length(x$files),

    n_published = sum(published),

    display_outdated = if (any(outdated)) TRUE else FALSE,
    n_outdated = if (any(outdated)) sum(outdated) else NA,
    f_outdated = if (any(outdated)) display_files(x$files[outdated]) else NA,

    display_unpublished = if (any(unpublished)) TRUE else FALSE,
    n_unpublished = if (any(unpublished)) sum(unpublished) else NA,
    f_unpublished = if (any(unpublished)) display_files(x$files[unpublished]) else NA,

    display_staged = if (any(staged)) TRUE else FALSE,
    n_staged = if (any(staged)) sum(staged) else NA,
    f_staged = if (any(staged)) display_files(x$files[staged]) else NA,

    display_unstaged = if (any(unstaged)) TRUE else FALSE,
    n_unstaged = if (any(unstaged)) sum(unstaged) else NA,
    f_unstaged = if (any(unstaged)) display_files(x$files[unstaged]) else NA,

    display_untracked = if (any(untracked)) TRUE else FALSE,
    n_untracked = if (any(untracked)) sum(untracked) else NA,
    f_untracked = if (any(untracked)) display_files(x$files[untracked]) else NA,

    display_ignored = if (any(ignored)) TRUE else FALSE,
    n_ignored = if (any(ignored)) sum(ignored) else NA,
    f_ignored = if (any(ignored)) display_files(x$files[ignored]) else NA)

  m <- whisker::whisker.render(template, variables)
  cat(m)
}

display_files <- function(files) {
  out <- "c("
  for (f in files) {
    out <- paste0(out, "\"", f, "\",\n")
  }
  out <- stringr::str_sub(out, 1, nchar(out) - 2)
  out <- paste0(out, ")")
  return(out)
}
