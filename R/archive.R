#' Archive objects
#'
#' \code{wflow_archive} archives objects from a specific version (i.e. Git
#' commit) of the workflowr project.
#'
#' \code{wflow_archive} serializes each object to a separate file using
#' \code{\link{saveRDS}}. To maintain organization and reduce the possibility of
#' conflict, the default is to save the objects within a subdirectory of the
#' archive directory which is named after the R Markdown file being knit.
#'
#' Note that this function uses non-standard evaluation to save the objects. Do
#' not use \code{wflow_archive} within a function (please open an Issue if it
#' would be useful to you to have a standard evaluation option for use within
#' functions).
#'
#' @param ... Objects to be archived.
#' @param archive_dir character (default: "../archive"). The path to the main
#'   archive directory.
#' @param subdir character (default: NULL). The name of the subdirectory to save
#'   the objects. If NULL, the name of the R Markdown file is used.
#' @param overwrite logical (default: FALSE). If the object already has an
#'   archived file for the current version of the workflowr project, should it
#'   be overwritten?
#' @param path character (default: "."). By default the function assumes the
#'   current working directory is within the project. If this is not true,
#'   you'll need to provide the path to the project directory.
#'
#' @examples
#' \dontrun{
#' x <- 1
#' y <- 2
#' z <- 3
#' wflow_archive(x, y, z)
#' }
#' @seealso \code{\link{wflow_restore}}, \code{\link{saveRDS}}
#' @export
wflow_archive <- function(...,
                          archive_dir = "../archive",
                          subdir = NULL,
                          overwrite = FALSE,
                          path = ".") {
  if (!is.character(archive_dir) | length(archive_dir) != 1)
    stop("archive_dir must be a one element character vector: ", archive_dir)
  if (!is.null(subdir) | !is.character(archive_dir) & length(archive_dir) != 1)
    stop("subdir must be NULL or a one element character vector: ", subdir)
  if (!is.logical(overwrite) | length(overwrite) != 1)
    stop("overwrite must be one element logical vector")
  if (!is.character(path) | length(path) != 1)
    stop("path must be a one element character vector: ", path)

  if (!git2r::in_repository(path))
    stop("wflow_archive requires a Git repository.",
         "\nPlease update path: ", path)

  if (!dir.exists(archive_dir)) {
    dir.create(archive_dir, recursive = TRUE)
    message("Archive directory created: ", archive_dir)
  }

  if (is.null(subdir)) {
    rmd_file <- knitr::current_input()
    if (is.null(rmd_file)) {
      stop("wflow_archive must be knit in an R Markdown document")
    } else {
      subdir <- basename(rmd_file)
    }
  }
  location <- file.path(archive_dir, subdir)
  if (!dir.exists(location)) {
    dir.create(location, recursive = TRUE)
    message("Archive subdirectory created: ", location)
  }
  message("Archive files will be saved in ", location)

  id <- extract_commit(path, 1)$sha1
  if (is.na(id))
    stop("No commit ID (sha1) available in this Git repository.",
         "\nPlease make at least one commit first.")

  archive(..., id = id, location = location, overwrite = overwrite)
}

archive <- function(..., id, location, overwrite = FALSE) {
  objs <- list(...)
  names(objs) <- as.list(substitute(list(...)))[-1L]

  if (length(objs) == 0)
    stop("No objects to archive")
  if (!is.character(id) | length(id) > 1)
    stop("id must be a one element character vector")
  if (grepl("-", id))
    stop("id cannot contain dashes: ", id)
  if (grepl(" ", id))
    stop("id cannot contain spaces: ", id)
  if (!is.character(location) | length(location) > 1)
    stop("location must be a one element character vector")
  if (!dir.exists(location))
    stop("location must already exist: ", location)
  if (!is.logical(overwrite) | length(overwrite) != 1)
    stop("overwrite must be one element logical vector")

  for (i in seq_along(objs)) {
    fname_base <- paste0(names(objs)[i], "-", id, ".rds")
    fname = file.path(location, fname_base)
    if (file.exists(fname) & !overwrite) {
      warning("Archive file already exists: ", fname,
              "\nSet `overwrite = TRUE` to overwrite.",
              call. = FALSE)
    } else if (file.exists(fname) & overwrite) {
      warning("Overwriting existing archive file: ", fname,
              call. = FALSE)
      saveRDS(objs[[i]], file = fname)
    } else {
      saveRDS(objs[[i]], file = fname)
    }
  }
  return(invisible(names(objs)))
}

#' Restore objects
#'
#' \code{wflow_restore} restores all previous versions of objects which had been
#' archived with \code{wflow_archive}.
#'
#' \code{wflow_restore} loads each serialized object to a separate file using
#' \code{\link{readRDS}}. By default it searches for a subdirectory within the
#' archive directory which is named after the R Markdown file being knit.
#'
#' Note that this function uses non-standard evaluation to save the objects. Do
#' not use \code{wflow_restore} within a function (please open an Issue if it
#' would be useful to you to have a standard evaluation option for use within
#' functions).
#'
#' @param ... Objects to be archived.
#' @param archive_dir character (default: "../archive"). The path to the main
#'   archive directory.
#' @param subdir character (default: NULL). The name of the subdirectory to save
#'   the objects. If NULL, the name of the R Markdown file is used.
#' @param path character (default: "."). By default the function assumes the
#'   current working directory is within the project. If this is not true,
#'   you'll need to provide the path to the project directory.
#'
#' @return Returns a nested list. The top-level list has one element for each
#'   object that was restored. Each of these elements in turn are lists with one
#'   element per code version (i.e. Git commit ID).
#'
#' @examples
#' \dontrun{
#' restored <- wflow_restore(x, y, z)
#' }
#' @seealso \code{\link{wflow_archive}}, \code{\link{readRDS}}
#' @export
wflow_restore <- function(...,
                          archive_dir = "../archive",
                          subdir = NULL,
                          path = ".") {
  if (!is.character(archive_dir) | length(archive_dir) != 1)
    stop("archive_dir must be a one element character vector: ", archive_dir)
  if (!is.null(subdir) | !is.character(archive_dir) & length(archive_dir) != 1)
    stop("subdir must be NULL or a one element character vector: ", subdir)
  if (!is.character(path) | length(path) != 1)
    stop("path must be a one element character vector: ", path)

  if (!git2r::in_repository(path))
    stop("wflow_restore requires a Git repository.",
         "\nPlease update path: ", path)

  if (!dir.exists(archive_dir))
    stop("Archive directory does not exist: ", archive_dir)

  if (is.null(subdir)) {
    rmd_file <- knitr::current_input()
    if (is.null(rmd_file)) {
      stop("wflow_archive must be knit in an R Markdown document")
    } else {
      subdir <- basename(rmd_file)
    }
  }
  location <- file.path(archive_dir, subdir)
  if (!dir.exists(location)) {
    stop("Archive subdirectory does not exist: ", location)
  }
  message("Restoring files from ", location)

  restore(...,  location = location)
}

restore <- function(..., location) {
  obj_names <- as.list(substitute(list(...)))[-1L]
  obj_names <- as.character(obj_names)

  if (length(obj_names) == 0)
    stop("No objects to resurrect")
  if (!is.character(location) | length(location) > 1)
    stop("location must be a one element character vector")
  if (!dir.exists(location))
    stop("location must already exist: ", location)

  result <- list()
  for (i in seq_along(obj_names)) {
    obj_files <- list.files(path = location, pattern = sprintf("^%s-.*.rds$",
                                                               obj_names[i]))

    if (length(obj_files) == 0) {
      warning("Could not find archived values of ", obj_names[i], " in ",
              location)
      result[[obj_names[i]]] <- NA
      next()
    }

    result[[obj_names[i]]] <- list()
    for (j in seq_along(obj_files)) {
      id <- stringr::str_split_fixed(stringr::str_replace(obj_files[j], ".rds$", ""),
                                     "-", 2)[, 2]
      result[[obj_names[i]]][[id]] <- readRDS(file.path(location, obj_files[j]))
    }
  }
  return(result)
}
