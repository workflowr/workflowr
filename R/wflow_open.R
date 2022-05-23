#' Open R Markdown analysis file(s)
#'
#' \code{wflow_open} opens R Markdown files in RStudio and sets the working
#' directory to the knit directory (see Details). If a file does not exist, a
#' minimal one is created.
#'
#' \code{wflow_open} is a convenience function to make it easier to begin
#' working, especially when starting a new analysis. First, it creates a new
#' file if necessary and tries to make educated guesses about metadata like the
#' title, author, and date. Second, it sets the working directory to the knit
#' directory. The knit directory is where the code in the R Markdown files is
#' executed, and may be defined via the field \code{knit_root_dir} in the file
#' \code{_workflowr.yml} (see \code{\link{wflow_html}} for all the details). If
#' this field is not defined, then the knit directory is the R Markdown
#' directory. Third, it opens the file(s) in RStudio if applicable. The latter
#' two side effects can be turned off if desired.
#'
#' If you would like to create an R Markdown file with \code{wflow_open} for an
#' analysis that is not part of a workflowr project, set \code{project = NULL}.
#' Otherwise \code{wflow_open} will throw an error. Note that the working
#' directory is \bold{not} changed when \code{project = NULL}.
#'
#' @param files character. R Markdown file(s) to open. Files must have the
#'   extension Rmd or rmd. Supports file
#'   \href{https://en.wikipedia.org/wiki/Glob_(programming)}{globbing}. Set
#'   \code{project = NULL} to create an R Markdown file outside of the R
#'   Markdown directory of a workflowr project.
#' @param change_wd logical (default: TRUE). Change the working directory to the
#'   knit directory. If \code{project = NULL}, the working directory is
#'   \bold{not} changed.
#' @param edit_in_rstudio logical (default: TRUE). Open the file(s) in the
#'   RStudio editor.
#' @param project character (or NULL). By default the function assumes the
#'   current working directory is within the project. If this is not true,
#'   you'll need to provide the path to the project directory. Set \code{project
#'   = NULL} if running this command to create a file for a non-workflowr
#'   project.
#'
#' @return An object of class \code{wflow_open}, which is a list with the
#'   following elements:
#'
#'   \item{files}{The input argument \code{files} as absolute paths.}
#'
#'   \item{change_wd}{The input argument \code{change_wd}.}
#'
#'   \item{edit_in_rstudio}{The input argument \code{edit_in_rstudio}.}
#'
#'   \item{knit_root_dir}{The knit directory (see \code{\link{wflow_html}} for
#'   details). This is \code{NULL} if \code{project} was set to \code{NULL}.}
#'
#'   \item{previous_wd}{The working directory in which \code{wflow_open} was
#'   executed.}
#'
#'   \item{new_wd}{The working directory that \code{wflow_open} changed to. The
#'   value is \code{NULL} if the working directory was not changed.}
#'
#'   \item{files_new}{The subset of the input argument \code{files} that were
#'   newly created. Paths are absolute.}
#'
#' @examples
#' \dontrun{
#' wflow_open("analysis/model-data.Rmd")
#' # Multiple files
#' wflow_open(c("analysis/model-data.Rmd", "analysis/another-analysis.Rmd"))
#' # Open all R Markdown files
#' wflow_open("analysis/*Rmd")
#' # Create an R Markdown file in a non-worklowr project
#' wflow_open("model-data.Rmd", project = NULL)
#' }
#' @import rmarkdown
#' @export
wflow_open <- function(files,
                       change_wd = TRUE,
                       edit_in_rstudio = TRUE,
                       project = ".") {

  # Check input arguments ------------------------------------------------------

  files <- process_input_files(files, rmd_only = TRUE, must_exist = FALSE)
  files <- absolute(files)
  assert_is_flag(change_wd)
  assert_is_flag(edit_in_rstudio)
  check_wd_exists()

  if (!is.null(project)) {
    assert_is_single_directory(project)
    project <- absolute(project)
  }

  files_new <- files[!fs::file_exists(files)]

  # Determine knit directory ---------------------------------------------------

  # If project is NULL, set `knit_directory` to NULL
  if (is.null(project)) {
    knit_directory <- NULL
  } else {
    # If project is set, find the knit directory

    # Confirm project is a valid workflowr project
    tryCatch(p <- wflow_paths(project = project),
             error = function(e) {
               stop(call. = FALSE, wrap(
                 "This isn't a workflowr project. Set project=NULL to create
                 an R Markdown file outside of a workflowr project. See
                 ?wflow_open for details."
               ))
             }
    )

    # Send a warning if user has a beta workflowr project and tries to create a
    # new file
    yml_index <- rmarkdown::yaml_front_matter(file.path(p$analysis, "index.Rmd"))
    usingBeta <- is.null(yml_index[["site"]]) || yml_index[["site"]] != "workflowr::wflow_site"
    if (usingBeta && length(files_new) > 0) {
      warning(call. = FALSE, wrap(
        "It appears that your site was created using a beta release of
        workflowr, thus you likely don't want to use the R Markdown file
        created by wflow_open. It doesn't have the necessary template chunks.
        Here are you options:"
      ), "\n\n",
      "To continue using your site as is, you can:\n\n",
      "1. Create a new R Markdown file by copying an existing one\n\n",
      "2. Install the beta release of workflowr and use its wflow_open:\n\n",
      "remotes::install_github(\"jdblischak/workflowrBeta\")\n\n",
      "To update to a workflowr 1.0+ site, you can:\n\n",
      "1. Run wflow_update() to preview the files that would be affected\n\n",
      "2. Follow the instructions in ?wflow_update to make the transition\n\n",
      "(Note that wflow_update() is only available in workflowr <= 1.6.2)")
    }

    # Throw error if Rmd files not saved in R Markdown directory
    if (.Platform$OS.type == "windows") {
      non_analysis <- !absolute(dirname(files)) == absolute(p$analysis)
    } else {
      non_analysis <- !dirname(files) == absolute(p$analysis)
    }
    if (any(non_analysis)) {
      stop(call. = FALSE, wrap(
        "Argument \"files\" specifies at least one file outside the R Markdown
        source directory:"), "\n\n",
        paste(files[non_analysis], collapse = "\n"), "\n\n",
        wrap(
          "R Markdown files inside your workflowr project must be in the
          following directory:"), "\n\n",
        absolute(p$analysis),
        "\n\n",
        wrap(
          "Set project=NULL to create an R Markdown file outside of the R
          Markdown directory of your workflowr project. See ?wflow_open for
          details."
        )
      )
    }

    # Find knit directory (knit_root_dir). This ignores any potential settings
    # in the YAML header(s) of existing file(s).
    wflow_opts <- wflow_options(files[1])
    knit_directory <- wflow_opts$knit_root_dir
    if (is.null(knit_directory)) {
      knit_directory <- p$analysis
    }
    knit_directory <- absolute(knit_directory)
  }

  # Guess metadata -------------------------------------------------------------

  yaml_title <- tools::file_path_sans_ext(basename(files))

  yaml_author <- ""
  git_config <- git2r::config()
  if (!is.null(git_config$global$user.name)) {
    yaml_author <-  git_config$global$user.name
  }
  if (!is.null(git_config$local$user.name)) {
    yaml_author <-  git_config$local$user.name
  }

  yaml_date <- as.character(Sys.Date())

  # Create files ---------------------------------------------------------------

  for (i in seq_along(files_new)) {
    header <- glue::glue("---
                           title: \"{yaml_title[i]}\"
                           author: \"{yaml_author}\"
                           date: \"{yaml_date}\"
                           output: workflowr::wflow_html
                           editor_options:
                             chunk_output_type: console
                           ---")
    boilerplate <- c("",
                     "## Introduction",
                     "",
                     "```{r}",
                     "",
                     "```",
                     "")
    fs::dir_create(dirname(files_new[i]))
    writeLines(c(header, boilerplate), files_new[i])
  }

  # Now that files all exist, ensure that symlinks are expanded
  files <- absolute(files)
  files_new <- absolute(files_new)

  # Set working directory ------------------------------------------------------

  current_wd <- absolute(getwd())
  if (change_wd && !is.null(project) && current_wd != knit_directory) {
    setwd(knit_directory)
    new_wd <- knit_directory
  } else {
    new_wd <- NULL
  }

  # Open files -----------------------------------------------------------------

  if (rstudioapi::isAvailable() && edit_in_rstudio) {
    for (rmd in files) {
      rstudioapi::navigateToFile(rmd)
    }
  }

  # Prepare output -------------------------------------------------------------

  o <- list(files = files,
            change_wd = change_wd,
            edit_in_rstudio = edit_in_rstudio,
            knit_root_dir = knit_directory,
            previous_wd  = current_wd,
            new_wd = new_wd,
            files_new = files_new)

  class(o) <- "wflow_open"

  return(o)
}

#' @export
print.wflow_open <- function(x, ...) {
  cat("wflow_open:\n")
  if (length(x$files_new) > 0) {
    cat("- New file(s):\n")
    cat(paste0("  ", x$files_new), sep = "\n")
  }
  files_existing <- setdiff(x$files, x$files_new)
  if (length(files_existing) > 0) {
    cat("- Existing file(s):\n")
    cat(paste0("  ", files_existing), sep = "\n")
  }

  if (x$change_wd && !is.null(x$new_wd)) {
    cat(sprintf("- New working directory: %s\n", x$new_wd))
  } else {
    cat(sprintf("- Same working directory: %s\n", x$previous_wd))
  }

  return(invisible(x))
}
