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
#' \describe{
#'
#' \item{\code{\link{wflow_start}}}{Start a workflowr project.}
#'
#' \item{\code{\link{wflow_build}}}{Build the site to view locally.}
#'
#' \item{\code{\link{wflow_publish}}}{Publish analyses to include in the
#'       website.}
#'
#' \item{\code{\link{wflow_status}}}{Report status of analysis files.}
#' }
#'
#' @section Supporting workflowr functions:
#'
#' For further information on workflowr, see the help pages for these
#' functions:
#'
#' \describe{
#'
#' \item{\code{\link{wflow_html}}}{This help page gives more technical
#'   details about how R Markdown files are converted to webpages, and
#'   how the rendering settings can be customized.}
#' }
#'
#' @docType package
#' @name workflowr
#' 
NULL
