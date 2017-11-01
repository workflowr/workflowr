## This makes sure that R loads the workflowr package
## automatically, everytime the project is loaded
if (require("workflowr", quietly = T)) {
  message("Loading .Rprofile for the current workflowr project")
  library(workflowr)
} else {
  message("workflowr package not installed, please run
          devtools::install_github('jdblischak/workflowr')
          to use the workflowr functions")
}
