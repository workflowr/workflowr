# Resources:
# .onAttach & zzz.R explained: http://r-pkgs.had.co.nz/r.html#r-differences

.onAttach <- function(libname, pkgname) {
  m <- c(sprintf("This is workflowr version %s",
                 utils::packageVersion("workflowr")),
         "Please send bug reports and feature requests to:",
         "https://github.com/jdblischak/workflowr/issues")
  packageStartupMessage(paste(m, collapse = "\n"))
}

#' workflowr: A workflow template for creating a research website
#'
#' The workflowr package helps you create a research website using R Markdown
#' and Git.
#'
#' @section Vignettes:
#'
#'   Run \code{browseVignettes("workflowr")} to see the package vignettes.
#'
#' @section Main workflowr functions:
#'
#'   \itemize{
#'
#'   \item \code{\link{wflow_start}} - Start workflowr project
#'
#'   \item \code{\link{wflow_open}} - Open a new R Markdown file based on the
#'   workflowr template
#'
#'   \item \code{\link{wflow_publish}} - Publish analyses to include in the
#'   website
#'
#'   \item \code{\link{wflow_status}} - Report status of analysis files
#'
#'   }
#'
#' @docType package
#' @name workflowr
NULL
