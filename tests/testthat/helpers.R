# Functions to temporarily move global .gitconfig file if it exists

library("withr")

# Temporarily move global ~/.gitconfig file ------------------------------------

# withr "set" function
#
# Moves .gitconfig to .gitconfig-suffix
remove_gitconfig <- function(suffix) {
  user_home <- workflowr:::get_home()
  config_original <- file.path(user_home, ".gitconfig")
  config_tmp <- paste0(config_original, suffix)
  if (fs::file_exists(config_original)) {
    file.rename(from = config_original, to = config_tmp)
  }
  return(config_tmp)
}

# withr "reset" function
#
# Moves .gitconfig-suffix to .gitconfig, or deletes .gitconfig if it hadn't
# existed prior
restore_gitconfig <- function(config_tmp) {
  user_home <- workflowr:::get_home()
  config_original <- file.path(user_home, ".gitconfig")
  if (fs::file_exists(config_tmp)) {
    file.rename(from = config_tmp, to = config_original)
  } else if (fs::file_exists(config_original)) {
    # If no temporary backup was created, remove the .gitconfig just created
    fs::file_delete(config_original)
  }
}

local_no_gitconfig <- withr::local_(set = remove_gitconfig,
                                    reset = restore_gitconfig)
