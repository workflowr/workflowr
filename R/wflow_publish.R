# To eventually replace current functionality of wflow_commit
# wflow_publish <- function() {
#   # wflow_commit_()
#   # wflow_build()
#   # wflow_commit_()
# }

# Wrapper for git2r::commit
wflow_commit_ <- function(files = NULL, message = NULL, all = FALSE,
                          force = FALSE, dry_run = FALSE, path = ".") {
  if (!(is.null(files) | is.character(files)))
    stop("files must be NULL or a character vector")
  if (!(is.null(message) | is.character(message)))
    stop("files must be NULL or a character vector")
  if (!is.logical(all) | length(all) != 1)
    stop("all must be a one-element logical vector")
  if (!is.logical(dry_run) | length(dry_run) != 1)
    stop("dry_run must be a one-element logical vector")
  if (!is.character(path) | length(path) != 1)
    stop("path must be a one element character vector")
  if (!dir.exists(path))
    stop("path does not exist.")

  if (is.null(files) && !all)
    stop("Must specify files to commit, set `all = TRUE`, or both",
         call. = FALSE)

  # Establish connection to Git repository
  path <- normalizePath(path)
  root_path <- try(rprojroot::find_rstudio_root_file(path = path))
  if (class(root_path) == "try-error")
    stop("Unable to find RStudio Rproj file ",
         "at the root of the workflowr project",
         call. = FALSE)
  r <- try(git2r::repository(root_path, discover = TRUE))
  if (class(r) == "try-error")
    stop("Unable to locate Git repository in ", path, call. = FALSE)
  if (root_path != dirname(r@path))
    warning("The Git repository is not at the root of the workflowr project",
            "\nworkflowr project: ", root_path,
            "\nGit repo: ", dirname(r@path),
            call. = FALSE)
  status <- git2r::status(r, ignored = TRUE)

  # Collate files to be added and committed
  if (!is.null(files)) {
    # Identify non-existent files
    files_exist <- file.exists(files)
    if (length(files) == sum(!files_exist)) {
      stop("None of the files could be found. ",
           "Check your working directory and file paths.",
           call. = FALSE)
    } else if (any(!files_exist)) {
      warning("Non-existent files found.",
              "\nThe following files do not exist:\n\n",
              paste(files[!files_exist], collapse = "\n"),
              call. = FALSE)
      files <- files[files_exist]
    }
    files <- normalizePath(files)
    # Identify files outside the Git repository
    files_outside <- !stringr::str_detect(files, root_path)
    if (length(files) == sum(files_outside)) {
      stop("All of the files are outside of the Git repository. ",
           "Check your working directory and file paths.",
           call. = FALSE)
    } else if (any(files_outside)) {
      warning("Some files are outside of the Git repository ",
              "and therefore will **not** be included in the commit.",
              "\nThe following files are outside the Git repository:\n\n",
              paste(files[files_outside], collapse = "\n"),
              call. = FALSE)
      files <- files[!files_outside]
    }
    # Refuse to commit any untracked file larger than 100MB.
    files_untracked_all <- c(unlist(status$untracked),
                             unlist(status$ignored))
    files_untracked_all <- normalizePath(files_untracked_all)
    is_untracked <- files %in% files_untracked_all
    sizes <- file.size(files) / 10^6
    ok_size <- sizes < 100
    files_too_large <- is_untracked & !ok_size
    if (length(files) == sum(files_too_large)) {
      stop("Files must be less than 100 MB to push to GitHub. ",
           "None of the files meet this requirement. ",
           "Run Git from the commandline if you ",
           "really want to commit these files.",
           call. = FALSE)
    } else if (any(files_too_large)) {
      warning("Some files are larger than GitHub's limit of 100 MB ",
              "and therefore will **not** be included in the commit. ",
              "Run Git from the commandline if you ",
              "really want to commit these files.",
              "\nThe following files are larger than 100 MB:\n\n",
              paste(files[files_too_large], collapse = "\n"),
              call. = FALSE)
      files <- files[!files_too_large]
    }
  }

  # Send warning if staged files are present but not specified to be committed
  files_staged <- unlist(status$staged)
  if (length(files_staged) != 0) {
    files_staged_unexpected <- setdiff(normalizePath(files_staged), files)
    if (length(files_staged_unexpected) != 0)
      warning("There are unexpected staged changes ",
              "that will be included in commit.",
              "\nSpecifically the following files have staged changes:\n\n",
              paste(normalizePath(files_staged_unexpected), collapse = "\n"),
              call. = FALSE)
  }

  # Prepare commit message
  if (is.null(message)) {
    warning("It is strongly recommended to include a custom commit message.",
            call. = FALSE)
    message <- deparse(sys.call())
  } else {
    message <- paste(message, collapse = "\n")
  }

  # Send message for dry run, otherwise add/commit files
  if (dry_run) {
    message("Dry run settings",
            "\n----",
            "\n\n* Specified files to add:\n\n",
            if (is.null(files)) "NA" else paste(files, collapse = "\n"),
            "\n\n* message:\n\n", message,
            "\n\n* Automatically stage files that have been ",
            "modified and deleted (all): ", all,
            "\n\n* Allow adding otherwise ignored files (force): ", force,
            "\n\n")
  } else {
    # Add the specified files
    if (!is.null(files)) {
      git2r::add(r, files, force = force)
      # Send warning if no files were staged
      status_post_add <- git2r::status(r, ignored = TRUE)
      files_staged_post_add <- unlist(status_post_add$staged)
      files_added <- setdiff(files_staged_post_add, files_staged)
      if (length(files_added) == 0)
        warning("None of the specified files were added to the staging area. ",
                "Perhaps they have not been updated ",
                "or the changes had already been added.",
                call. = FALSE)
    }
    # Commit
    tryCatch(git2r::commit(r, message = message, all = all),
             error = function(e) {
               if (stringr::str_detect(e$message, "Nothing added to commit")) {
                 reason <- "Commit failed because no files were added."
               } else {
                 reason <- "Commit failed for unknown reason."
               }
               stop(reason, " Any untracked files must manually specified",
                    " even if `all = TRUE`.\n\n", call. = FALSE)
               }
             )
  }

  return(invisible(files))
}
