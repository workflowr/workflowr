wflow_start_rstudio <- function(directory,
                                name = NULL,
                                git = TRUE,
                                existing = FALSE,
                                overwrite = FALSE,
                                change_wd = TRUE,
                                disable_remote = FALSE,
                                dry_run = FALSE,
                                user.name = NULL,
                                user.email = NULL) {

  #
  directory_rs <- directory

  # Check if name is blank, use NULL if true
  if (name == "") {
    name_rs <- NULL
  } else {
    name_rs <- name
  }

  git_rs <- git

  existing_rs <- existing

  overwrite_rs <- overwrite

  change_wd_rs <- change_wd

  disable_remote_rs <- disable_remote

  dry_run_rs <- dry_run

  # Check if user.name is blank, use NULL if true
  if (user.name == "") {
    user.name_rs <- NULL
    warning("No user name is recorded for the project, the global setting will be used")
  } else {
    user.name_rs <- user.name
  }

  # Check if user.email is blank, use NULL if true
  if (user.email == "") {
    user.email_rs <- NULL
    warning("No user email is recorded for the project, the global setting will be used")
  } else {
    user.email_rs <- user.email
  }

  ## dev: print out the checked variable
  # print(directory_rs)
  # print(name_rs)
  # print(git_rs)
  # print(existing_rs)
  # print(overwrite_rs)
  # print(change_wd_rs)
  # print(disable_remote_rs)
  # print(dry_run_rs)
  # print(user.name_rs)
  # print(user.email_rs)

  wflow_start(directory = directory_rs,
              name = name_rs,
              git = git_rs,
              existing = existing_rs,
              overwrite = overwrite_rs,
              change_wd = change_wd_rs,
              disable_remote = disable_remote_rs,
              dry_run = dry_run_rs,
              user.name = user.name_rs,
              user.email = user.email_rs)
}
