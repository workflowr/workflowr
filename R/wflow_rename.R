#' Rename files and directories
#'
#' \code{wflow_rename} renames files and directories. If the file to be renamed
#' is an R Markdown file, the corresponding HTML and other related files are
#' also renamed. If the workflowr project uses Git, \code{wflow_rename} commits
#' the changes.
#'
#' @param files character. Files to be renamed. Supports file
#'   \href{https://en.wikipedia.org/wiki/Glob_(programming)}{globbing}.
#' @param to character. New names for the files. Must be the same length as
#'   \code{files}.
#' @param git logical (default: TRUE). Commit the changes (only applicable if
#'   Git repository is present).
#' @param dry_run logical (default: FALSE). Preview the files to be renamed but
#'   do not actually rename them.
#' @inheritParams wflow_git_commit
#'
#' @return An object of class \code{wflow_rename}, which is a list with the
#'   following elements:
#'
#'   \itemize{
#'
#'   \item \bold{files}: The relative path(s) to the renamed file(s).
#'
#'   \item \bold{to}: The new relative path(s) to rename the file(s).
#'
#'   \item \bold{message}: The message describing the commit (if applicable).
#'
#'   \item \bold{git}: The input argument \code{git}.
#'
#'   \item \bold{dry_run}: The input argument \code{dry_run}.
#'
#'   \item \bold{commit}:The object returned by
#'   \link{git2r}::\code{\link[git2r]{commit}} (only included if \code{dry_run
#'   == FALSE}).
#'
#'   \item \bold{files_git}: The relative path(s) to the file(s) renamed from
#'   the Git repository.
#'
#'   }
#'
#' @seealso \code{\link{wflow_git_commit}}
#'
#' @examples
#' \dontrun{
#'
#' # rename a single file
#' wflow_rename("analysis/file.Rmd", "analysis/new.Rmd", "rename old analysis.")
#' # rename multiple files
#' wflow_rename(c("analysis/file.Rmd", "output/small-data.txt"),
#'              c("analysis/new.Rmd", "output/new-data.txt"),
#'              "rename old analysis and its associated data.")
#' }
#'
#' @export
wflow_rename <- function(files,
                         to,
                         message = NULL,
                         git = TRUE,
                         dry_run = FALSE,
                         project = ".") {

  # Check input arguments ------------------------------------------------------

  if (!(is.character(files) && length(files) > 0))
    stop("files must be a character vector of filenames")
  files <- glob(files)
  if (!all(fs::file_exists(files) | fs::dir_exists(files)))
    stop("Not all files exist. Check the paths to the files")
  # Change filepaths to relative paths
  files <- relative(files)

  if (!(is.character(to) && length(to) == length(files)))
    stop("to must be a character vector of filenames the same length as files")
  # Warning: this will not resolve symlinks since the files do not yet exist
  to <- relative(to)

  if (is.null(message)) {
    message <- deparse(sys.call())
    message <- paste(message, collapse = "\n")
  } else if (is.character(message)) {
    message <- wrap(paste(message, collapse = " "))
  } else {
    stop("message must be NULL or a character vector")
  }

  if (!(is.logical(git) && length(git) == 1))
    stop("git must be a one-element logical vector")

  if (!(is.logical(dry_run) && length(dry_run) == 1))
    stop("dry_run must be a one-element logical vector")

  if (!(is.character(project) && length(project) == 1))
    stop("project must be a one-element character vector")

  check_wd_exists()

  if (!fs::dir_exists(project)) {
    stop("project directory does not exist.")
  }

  project <- absolute(project)

  # Assess project status ------------------------------------------------------

  p <- wflow_paths(project = project)

  # Should changes be committed?
  use_git <- !is.na(p$git) && git

  # Even if not committing files, still assess the Git repository
  if (!is.na(p$git)) {
    r <- git2r::repository(path = p$git)
  }

  # Early stops
  if (use_git && !dry_run) {
    # Git must be configured
    check_git_config(project, "`wflow_rename` with `git = TRUE`")
    # No staged files
    check_staged_changes(project, "`wflow_rename` with `git = TRUE`")
  }
  # No HTML files in website directory

  # No figures or figure directories

  # from and to must have same file extensions

  # Rmd file in analyis/ must be renamed to same diretory

  # Gather R Markdown accessory files to rename --------------------------------

  # Are any of the specified files R Markdown files in the analysis directory?
  files_ext <- tools::file_ext(files)
  rmd <- which(files_ext %in% c("Rmd", "rmd") &
    absolute(files) == absolute(file.path(p$analysis, basename(files))))

  for (i in rmd) {
    # Corresponding HTML?
    html1 <- to_html(files[i], outdir = p$docs)
    html2 <- to_html(to[i], outdir = p$docs)
    if (fs::file_exists(html1)) {
      files <- c(files, html1)
      to <- c(to, html2)
    }
    # Any figure files in docs directory?
    if (p$docs == ".") {
      dir_figs_docs1 <- file.path("figure", basename(files[i]))
      dir_figs_docs2 <- file.path("figure", basename(to[i]))
    } else {
      dir_figs_docs1 <- file.path(p$docs, "figure", basename(files[i]))
      dir_figs_docs2 <- file.path(p$docs, "figure", basename(to[i]))
    }
    if (fs::dir_exists(dir_figs_docs1)) {
      files <- c(files, dir_figs_docs1)
      to <- c(to, dir_figs_docs2)
    }
  }

  # Expand directories ---------------------------------------------------------

  is_dir <- fs::dir_exists(files)
  dirs_from <- files[is_dir]
  dirs_to <- to[is_dir]
  files <- files[!is_dir]
  to <- to[!is_dir]
  for (i in seq_along(dirs_from)) {
    d_files_from <- list.files(path = dirs_from[i], all.files = TRUE,
                               full.names = TRUE, recursive = TRUE)
    # Replace with new directory name
    d_files_to <- stringr::str_replace(d_files_from, dirs_from[i], dirs_to[i])
    files <- c(files, d_files_from)
    to <- c(to, d_files_to)
  }

  # Gather files to commit -----------------------------------------------------

  if (!is.na(p$git)) {
    # Obtain committed files
    files_committed <- relative(get_committed_files(r))

    # Obtain files to commit
    logical_files_git <- files %in% files_committed
    files_to_commit <- c(files[logical_files_git], to[logical_files_git])
  } else {
    files_to_commit <- NA_character_
  }

  # rename files ---------------------------------------------------------------

  if (!dry_run) {
    # Create any new directories, otherwise rename would not work
    lapply(to, function(x) {
      fs::dir_create(dirname(x))
    })
    # Rename individual files
    fs::file_move(path = files, new_path = to)
    # Remove any previous directories
    lapply(dirs_from, unlink, recursive = TRUE)
  }

  # Commit renamed files -------------------------------------------------------

  if (use_git && !dry_run && length(files_to_commit) > 0) {
    git2r_add(r, files_to_commit)
    git2r::commit(r, message = message)
    commit <- git2r::commits(r, n = 1)[[1]]
  } else {
    commit <- NA
  }

  # Prepare output -------------------------------------------------------------

  o <- list(files = files,
            to = to,
            message = message,
            git = git,
            dry_run = dry_run,
            commit = commit,
            # Re-run relative() on files_to_commit to resolve any potential
            # symlinks in paths to newly created files
            files_git = relative(files_to_commit))
  class(o) <- "wflow_rename"
  return(o)
}

#' @export
print.wflow_rename <- function(x, ...) {
  cat("Summary from wflow_rename\n\n")
  if (x$dry_run) {
    cat(wrap("The following file(s) would be renamed:"), "\n\n")
  } else {
    cat(wrap("The following files(s) were renamed:"), "\n\n")
  }
  cat(sprintf("%s -> %s", x$files, x$to), sep = "\n")

  if (length(x$files_git) > 0 && !all(is.na(x$files_git)) && x$git) {
    if (x$dry_run) {
      cat("\n", wrap("The following file(s) would be included in the Git commit:"),
          "\n\n", sep = "")
    } else {
      cat("\n", wrap(sprintf(
        "The following file(s) were included in Git commit %s:",
        stringr::str_sub(x$commit$sha, start = 1, end = 7))),
        "\n\n", sep = "")
    }
    cat(x$files_git, sep = "\n")
    cat("\ncommit message:\n")
    cat(x$message)
    cat("\n")
  }

  return(invisible(x))
}
