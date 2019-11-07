# Resources:
# .onAttach & zzz.R explained: http://r-pkgs.had.co.nz/r.html#r-differences

.onAttach <- function(libname, pkgname) {
  m <- c(sprintf("This is workflowr version %s",
                 utils::packageVersion("workflowr")),
         "Run ?workflowr for help getting started")
  packageStartupMessage(paste(m, collapse = "\n"))
  check_deps("workflowr")
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

# Check minimum required versions of dependencies b/c install.packages() doesn't
check_deps <- function(pkg) {
  deps <- utils::packageDescription(pkg, fields = "Imports")
  deps <- stringr::str_split(deps, pattern = ",[\n]")[[1]]
  deps <- stringr::str_split_fixed(deps, "\\s", n = 2)
  deps_names <- deps[, 1]
  deps_versions <- stringr::str_extract(deps[, 2], "[0-9,\\.,-]+")

  invalid <- character()
  for (i in seq_along(deps_names)) {
    pkgname <- deps_names[i]
    required <- deps_versions[i]
    if (is.na(required)) next

    installed <- utils::packageVersion(deps_names[i])

    if (as.numeric_version(installed) < as.numeric_version(required)) {
      warning(call. = FALSE, glue::glue(
        "Please update package \"{pkgname}\": version {installed} is installed, but {required} is required"
      ))
      invalid <- c(invalid, pkgname)
    }
  }

  return(invisible(invalid))
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
#'
NULL
