## Wrapper function to enhance RStudio Project Template

wflow_start_rstudio <- function(directory,
                                name = "",
                                git = TRUE,
                                existing = FALSE,
                                overwrite = FALSE,
                                user.name = "",
                                user.email = "") {

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

  # Check if user.name is blank, use NULL if true
  if (user.name == "") {
    user.name_rs <- NULL
    check_git_config(directory, custom_message = "the RStudio Project Template")
  } else {
    user.name_rs <- user.name
  }

  # Check if user.email is blank, use NULL if true
  if (user.email == "") {
    user.email_rs <- NULL
    check_git_config(directory, custom_message = "the RStudio Project Template")
  } else {
    user.email_rs <- user.email
  }

  wflow_start(directory = directory_rs,
              name = name_rs,
              git = git_rs,
              existing = existing_rs,
              overwrite = overwrite_rs,
              user.name = user.name_rs,
              user.email = user.email_rs)
}
