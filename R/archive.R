

wflow_archive <- function(...,
                          archive_dir = "../archive",
                          subdir = NULL,
                          overwrite = FALSE,
                          path = ".") {
  if (!is.character(path) | length(path) != 1) {
    stop(sprintf("The argument path must be a "))
  }


  if (!dir.exists(path)) {
    dir.create(path)
    message("Archive directory created: ", path)
  }
  saveRDS_archive <- function(o) {
    fname_base <- paste0(names(o), ".rds")
    fname = file.path(archive_dir, fname_base)
    saveRDS(o[[1]], file = fname)
  }
  for (i in seq_along(objs)) {
    saveRDS_archive(objs[i])
  }

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
    stop("location must already exist: ", "location")
  if (!is.logical(overwrite) | length(overwrite) != 1)
    stop("overwrite must be one element logical vector")

  for (i in seq_along(objs)) {
    fname_base <- paste0(names(objs)[i], "-", id, ".rds")
    fname = file.path(location, fname_base)
    if (file.exists(fname) & !overwrite) {
      warning("Archive file already exists: ", fname)
    } else {
      saveRDS(objs[[i]], file = fname)
    }
  }
}


restore <- function(..., location) {
  obj_names <- as.list(substitute(list(...)))[-1L]
  obj_names <- as.character(obj_names)

  if (length(obj_names) == 0)
    stop("No objects to resurrect")
  if (!is.character(location) | length(location) > 1)
    stop("location must be a one element character vector")
  if (!dir.exists(location))
    stop("location must already exist: ", "location")

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
