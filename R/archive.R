
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
