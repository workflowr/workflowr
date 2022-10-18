# Resources:
# .onAttach & zzz.R explained: https://r-pkgs.org/r.html#when-you-do-need-side-effects

.onAttach <- function(libname, pkgname) {
  m <- c(sprintf("This is workflowr version %s",
                 utils::packageVersion("workflowr")),
         "Run ?workflowr for help getting started")
  packageStartupMessage(paste(m, collapse = "\n"))
  check_dependencies()
}

.onLoad <- function(libname, pkgname) {
  sysgit <- Sys.which("git")
  wflow_pkg_opts <- list(
    workflowr.autosave = TRUE,
    workflowr.sysgit = if(fs::file_exists(sysgit)) sysgit else "",
    workflowr.view = interactive()
  )

  op <- options()
  toset <- !(names(wflow_pkg_opts) %in% names(op))
  if(any(toset)) options(wflow_pkg_opts[toset])

  invisible()
}

# Unfortunately I can't assume anything about the dependencies. They may be:
#
# * Not installed: a user can remove them after installing workflowr
# * Installed but unusable, e.g. one of their dependencies was removed
# * Installed but below the minimum required version
dependencies <- c(
  callr = "3.7.0",
  fs = "1.2.7",
  getPass = NA,
  git2r = "0.26.0",
  glue = NA,
  httpuv = "1.2.2",
  httr = NA,
  knitr = "1.29",
  rmarkdown = "1.18",
  rprojroot = "1.2",
  rstudioapi = "0.6",
  stringr = "1.3.0",
  tools = NA,
  utils = NA,
  whisker = "0.3-2",
  xfun = NA,
  yaml = NA
)
check_dependencies <- function() {
  for (i in seq_along(dependencies)) {
    pkg_name <- names(dependencies)[i]
    pkg_version <- dependencies[i]

    if (length(find.package(pkg_name, quiet = TRUE, verbose = FALSE)) == 0) {
      stop(sprintf("The required dependency \"%s\" is missing, please install it.",
                   pkg_name))
    }

    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      stop(sprintf("The required dependency \"%s\" is unable to be loaded, please re-install it.",
                   pkg_name))
    }

    if (!is.na(pkg_version)) {
      installed_version <- utils::packageVersion(pkg_name)
      if (installed_version < as.package_version(pkg_version)) {
        stop(sprintf(
          "Please update package \"%s\": version %s is installed, but %s is required",
          pkg_name, installed_version, pkg_version
        ))
      }
    }
  }

  return(NULL)
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
#'   \url{https://workflowr.github.io/workflowr/}.
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
#' \item{\code{\link{wflow_html}}}{More technical details about how
#'   individual R Markdown files are converted to webpages, and how the
#'   rendering settings can be customized.}
#'
#' \item{\code{\link{wflow_site}}}{This help page explains how
#'   project-wide rendering settings can be customized in the
#'   \code{_site.yml} file.}
#' }
#'
#' @section Package options:
#'
#' The following package options affect the default behavior of the workflowr
#' functions. To permanently set any of these options, add a call to the
#' function \code{\link[base]{options}} in the file \code{.Rprofile} at the root
#' of your workflowr project. For example:
#'
#' \preformatted{
#' # Do not use Git executable
#' options(workflowr.sysgit = "")
#' }
#'
#' \describe{
#'
#' \item{workflowr.autosave}{A logical indicating whether workflowr functions
#' should automatically save files open in the RStudio editor before running.
#' The default is \code{TRUE}. This requires RStudio 1.1.287 or later. Only
#' files that have been previously saved are affected. In other words, unnamed
#' files will be ignored.}
#'
#' \item{workflowr.sysgit}{The path to the system Git executable. This is
#' occasionally used to increase the speed of Git operations performed by
#' workflowr. By default it is set to the first Git executable on the search
#' path. You can specify a path to a different Git executable. Alternatively you
#' can disable this behavior entirely by setting it to the empty string \code{""}.}
#'
#' \item{workflowr.view}{A logical indicating whether workflowr functions should
#' open webpages for viewing in the browser. The default is set to
#' \code{\link[base]{interactive}} (i.e. it is \code{TRUE} only if it is an
#' interactive R session). This option is currently used by
#' \code{\link{wflow_build}}, \code{\link{wflow_git_push}}, and
#' \code{\link{wflow_publish}}.}
#' }
#'
#' @docType package
#' @name workflowr
#' @keywords internal
#'
NULL
