# Resources:
# .onAttach & zzz.R explained: http://r-pkgs.had.co.nz/r.html#r-differences

.onAttach <- function(libname, pkgname) {
  m <- c(sprintf("This is workflowr version %s",
                 utils::packageVersion("workflowr")),
         "Run ?workflowr for help getting started")
  packageStartupMessage(paste(m, collapse = "\n"))
}

#' workflowr: A workflow template for creating a research website
#'
#' The workflowr package helps you create a research website using R Markdown
#' and Git.
#'
#' @section Vignettes:
#'
#'   Run \code{browseVignettes("workflowr")} to read the package vignettes
#'   locally. Alternatively you can read the documentation online at
#'   \url{https://jdblischak.github.io/workflowr}.
#'
#' @section Main workflowr functions:
#'
#'   \itemize{
#'
#'   \item \code{\link{wflow_start}} - Start workflowr project
#'
#'   \item \code{\link{wflow_build}} - Build the site to view locally
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
