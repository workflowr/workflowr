#' Start a new workflowr project
#'
#' \code{wflow_start} creates a minimal workflowr project. The default
#' behaviour is to add these files to a new directory, but it is also
#' possible to populate an already existing project. By default, it
#' also changes the working directory to the workflowr project.
#'
#' This is the initial function that organizes the infrastructure to create a
#' research website for your project. If you're using RStudio, you can
#' alternatively create a new workflowr project using the RStudio project
#' template. Go to "File" -> "New Project..." and select "workflowr project"
#' from the list of project types. In the future you can return to your project
#' by choosing "Open Project...". This will set the correct working directory in
#' the R console, switch the file navigator to the project, and configure the
#' Git pane. Note that while you do not need to use RStudio with workflowr, do
#' not delete the Rproj file because it is required by other functions.
#'
#' \code{wflow_start} populates the directory with the following files:
#'
#' \preformatted{
#' ├── .gitignore
#' ├── .Rprofile
#' ├── _workflowr.yml
#' ├── analysis/
#' │   ├── about.Rmd
#' │   ├── index.Rmd
#' │   ├── license.Rmd
#' │   └── _site.yml
#' ├── code/
#' │   ├── README.md
#' ├── data/
#' │   └── README.md
#' ├── docs/
#' ├── directory.Rproj
#' ├── output/
#' │   └── README.md
#' └── README.md
#' }
#'
#' The two \bold{required} subdirectories are \code{analysis/} and \code{docs/}.
#' These directories should never be removed from the workflowr project.
#'
#' \code{analysis/} contains all the source R Markdown files for implementing
#' the data analyses for your project. It also contains a special R Markdown
#' file, \code{index.Rmd}, that does not contain any R code, but will be used to
#' generate \code{index.html}, the homepage for your website. In addition, this
#' directory contains the important configuration files \code{_site.yml}, which
#' you can use to edit the theme, navigation bar, and other website aesthetics.
#' Do not delete \code{index.Rmd} or \code{_site.yml}.
#'
#' \code{docs/} contains all the HTML files for your website. The HTML files are
#' built from the R Markdown files in \code{analysis/}. Furthermore, any figures
#' created by the R Markdown files are saved here. Each of these figures is
#' saved according to the following pattern: \code{docs/figure/<insert Rmd
#' filename>/<insert chunk name>-#.png}, where \code{#} corresponds to which of
#' the plots the chunk generated (since one chunk can produce an arbitrary
#' number of plots).
#'
#' The workflowr-specific configuration file is \code{_workflowr.yml}. It will
#' apply the workflowr reproducibility checks consistently across all your R
#' Markdown files. The most critical setting is \code{knit_root_dir}, which
#' determines the directory where the files in \code{analysis/} will be
#' executed. The default is to execute the code in the root of the project where
#' \code{_workflowr.yml} is located (i.e. \code{"."}). To instead execute the
#' code from \code{analysis/}, change the setting to \code{knit_root_dir:
#' "analysis"}. See \code{\link{wflow_html}} for more details.
#'
#' Also required is the RStudio project file, in the example
#' above\code{directory.Rproj}. Even if you are not using RStudio, do not delete
#' this file because the workflowr functions rely on it to determine the root
#' directory of the project.
#'
#' The \bold{optional} directories are \code{data/}, \code{code/}, and
#' \code{output/}. These directories are suggestions for organizing your data
#' analysis project, but can be removed if you do not find them useful.
#'
#' \code{data/} is a directory for raw data files.
#'
#' \code{code/} is a directory for code that might not be appropriate to include
#' in R Markdown format (e.g. for pre-processing the data, or for long-running
#' code).
#'
#' \code{output/} is a directory for processed data files and other outputs
#' generated from the code and data. For example, scripts in \code{code/} that
#' pre-process raw data files from \code{data/} should save the processed data
#' files in \code{output/}.
#'
#' The \code{.Rprofile} file is a regular R script that is run once when the
#' project is opened. It contains the call \code{library("workflowr")}, ensuring
#' that workflowr is loaded automatically each time a workflowr-project is
#' opened.
#'
#' @param directory character. The directory for the project, e.g.
#'   "~/new-project". When \code{existing = FALSE}, the directory will
#'   be created.
#'
#' @param name character (default: NULL). Project name, e.g. "My Project". When
#'   \code{name = NULL}, the project name is automatically set based on the
#'   argument \code{directory}. For example, if \code{directory =
#'   "~/projects/myproject"}, then \code{name} is set to \code{"myproject"}.
#'   \code{name} is displayed on the site's navigation bar and the README.md.
#'
#' @param git logical (default: TRUE). Should the workflowr files be committed
#'   with Git? If no Git repository is detected, \code{wflow_start} will
#'   initialize the repository and make an initial commit. If there is already a
#'   Git repository, \code{wflow_start} will make an additional commit. In both
#'   cases, only files needed for the workflowr project will be included in the
#'   commit.
#'
#' @param existing logical (default: FALSE). Indicate if the specified
#'   \code{directory} already exists. The default prevents injecting the
#'   workflowr files into an unwanted location. Only set to TRUE if you wish to
#'   add the workflowr files to an existing project.
#' @param overwrite logical (default: FALSE). Control whether to overwrite
#'   existing files that have the same names as the workflowr files. Only
#'   relevant if \code{existing = TRUE}. Even if \code{overwrite = FALSE},
#'   existing files that have the same names as the workflowr files will still
#'   be committed if \code{git = TRUE}.
#' @param change_wd logical (default: TRUE). Change the working directory to the
#'   \code{directory}.
#' @param dry_run logical (default: FALSE). Preview the actions to be performed
#'   without executing them.
#' @param user.name character (default: NULL). The user name used by Git to sign
#'   commits, e.g. "My Name". This setting will only apply to this specific
#'   workflowr project being created. To create a global Git user name to be the
#'   default for this computer, instead use \code{\link{wflow_git_config}}.
#' @param user.email character (default: NULL). The email addresse used by Git
#'   to sign commits, e.g. "email@domain". This setting will only apply to this
#'   specific workflowr project being created. To create a global Git email
#'   address to be the default for this computer, instead use
#'   \code{\link{wflow_git_config}}.
#' @return An object of class \code{wflow_start}, which is a list with the
#'   following elements:
#'
#'  \itemize{
#'
#'    \item \bold{directory}: The input argument \code{directory}
#'
#'    \item \bold{name}: The input argument \code{name}
#'
#'    \item \bold{git}: The input argument \code{git}
#'
#'    \item \bold{existing}: The input argument \code{existing}
#'
#'    \item \bold{overwrite}: The input argument \code{overwrite}
#'
#'    \item \bold{change_wd}: The input argument \code{change_wd}
#'
#'    \item \bold{dry_run}: The input argument \code{dry_run}
#'
#'    \item \bold{user.name}: The input argument \code{user.name}
#'
#'    \item \bold{user.email}: The input argument \code{user.email}
#'
#'    \item \bold{commit}: The \code{\link[git2r]{git_commit-class}} object
#'    returned by \link{git2r} (\code{NULL} if \code{git = FALSE}).
#'
#'  }
#'
#' @seealso vignette("wflow-01-getting-started")
#'
#' @examples
#' \dontrun{
#'
#' wflow_start("path/to/new-project")
#'
#' # Provide a custom name for the project.
#' wflow_start("path/to/new-project", name = "My Project")
#'
#' # Preview what wflow_start would do
#' wflow_start("path/to/new-project", dry_run = TRUE)
#'
#' # Add workflowr files to an existing project.
#' wflow_start("path/to/current-project", existing = TRUE)
#'
#' # Add workflowr files to an existing project, but do not automatically
#' # commit them.
#' wflow_start("path/to/current-project", git = FALSE, existing = TRUE)
#' }
#' @export
wflow_start <- function(directory,
                        name = NULL,
                        git = TRUE,
                        existing = FALSE,
                        overwrite = FALSE,
                        change_wd = TRUE,
                        dry_run = FALSE,
                        user.name = NULL,
                        user.email = NULL) {
  if (!is.character(directory) | length(directory) != 1)
    stop("directory must be a one element character vector: ", directory)
  if (!(is.null(name) | (is.character(name) | length(name) != 1)))
    stop("name must be NULL or a one element character vector: ", name)
  if (!is.logical(git) | length(git) != 1)
    stop("git must be a one element logical vector: ", git)
  if (!is.logical(existing) | length(existing) != 1)
    stop("existing must be a one element logical vector: ", existing)
  if (!is.logical(overwrite) | length(overwrite) != 1)
    stop("overwrite must be a one element logical vector: ", overwrite)
  if (!is.logical(change_wd) | length(change_wd) != 1)
    stop("change_wd must be a one element logical vector: ", change_wd)
  if (!is.logical(dry_run) | length(dry_run) != 1)
    stop("dry_run must be a one element logical vector: ", dry_run)
  if (!(is.null(user.name) | (is.character(user.name) | length(user.name) != 1)))
    stop("user.name must be NULL or a one element character vector: ", user.name)
  if (!(is.null(user.email) | (is.character(user.email) | length(user.email) != 1)))
    stop("user.email must be NULL or a one element character vector: ", user.email)
  if ((is.null(user.name) && !is.null(user.email)) ||
      (!is.null(user.name) && is.null(user.email)))
    stop("Must specify both user.name and user.email, or neither.")

  if (!existing & dir.exists(directory)) {
    stop("Directory already exists. Set existing = TRUE if you wish to add workflowr files to an already existing project.")
  } else if (existing & !dir.exists(directory)) {
    stop("Directory does not exist. Set existing = FALSE to create a new directory for the workflowr files.")
  }

  directory <- absolute(directory)

  # A workflowr directory cannot be created within an existing Git repository if
  # git = TRUE & existing = FALSE.
  if (git & !existing) {
    # In order to check if location is within an existing Git repository, first
    # must obtain the most upstream existing directory
    dir_existing <- obtain_existing_path(directory)
    if (git2r::in_repository(dir_existing)) {
      r <- git2r::repository(dir_existing, discover = TRUE)
      stop("The directory where you have chosen to create a new workflowr directory is already within a Git repository. This is potentially dangerous. If you want to have a workflowr project created within this existing Git repository, re-run wflow_start with `git = FALSE` and then manually commit the new files. The following directory contains the existing .git directory: ", dirname(r@path))
    }
  }

  # Require that user.name and user.email be set locally or globally
  if (git && is.null(user.name) && is.null(user.email)) {
    check_git_config(path = directory)
  }

  # Create directory if it doesn't already exist
  if (!existing && !dir.exists(directory) && !dry_run) {
    dir.create(directory, recursive = TRUE)
  }

  # Convert to absolute path. Needs to be run again after creating the directory
  # because symlinks can only resolved for existing directories.
  directory <- absolute(directory)

  # Configure name of workflowr project
  if (is.null(name)) {
    name <- basename(directory)
  }

  # Get variables to interpolate into _workflowr.yml
  wflow_version <- as.character(utils::packageVersion("workflowr"))
  the_seed_to_set <- as.numeric(format(Sys.Date(), "%Y%m%d")) # YYYYMMDD

  # Add files ------------------------------------------------------------------

  # Use templates defined in R/templates.R
  names(templates)[which(names(templates) == "Rproj")] <-
    glue::glue("{basename(directory)}.Rproj")
  names(templates) <- file.path(directory, names(templates))
  project_files <- names(templates)

  # Create subdirectories
  dir.create.vectorized <- Vectorize(dir.create, vectorize.args = "path")
  dir.create.vectorized(file.path(directory, c("analysis", "code", "data",
                                               "docs", "output")),
                        showWarnings = FALSE)

  if (!dry_run) {
    for (fname in project_files) {
      if (!file.exists(fname) || overwrite) {
        cat(glue::glue(templates[[fname]]), file = fname)
      }
    }
  }

  # Create .nojekyll files in analysis/ and docs/ directories
  nojekyll_analysis <- file.path(directory, "analysis", ".nojekyll")
  nojekyll_docs <- file.path(directory, "docs", ".nojekyll")
  project_files <- c(project_files, nojekyll_analysis, nojekyll_docs)
  if (!dry_run) {
    file.create(nojekyll_analysis)
    file.create(nojekyll_docs)
  }

  # Configure, initialize, and commit ------------------------------------------

  # Configure RStudio
  rs_version <- check_rstudio_version()

  # Change working directory to workflowr project
  if (change_wd && !dry_run) {
    setwd(directory)
  }

  # Configure Git repository
  if (git && !dry_run) {
    if (git2r::in_repository(directory)) {
      warning("A .git directory already exists in ", directory)
    } else {
      git2r::init(directory)
    }
    repo <- git2r::repository(directory)
    # Set local user.name and user.email
    if (!is.null(user.name) && !is.null(user.email)) {
      git2r::config(repo, user.name = user.name, user.email = user.email)
    }
    # Make the first workflowr commit
    git2r::add(repo, project_files, force = TRUE)
    status <- git2r::status(repo)
    if (length(status$staged) == 0) {
      warning("No new workflowr files were committed.")
    } else{
      commit <- git2r::commit(repo, message = "Start workflowr project.")
    }
  }

  # Prepare output -------------------------------------------------------------

  o <- list(directory = directory,
            name = name,
            git = git,
            existing = existing,
            overwrite = overwrite,
            change_wd = change_wd,
            dry_run = dry_run,
            user.name = user.name,
            user.email = user.email,
            commit = if (exists("commit")) commit else NULL)
  class(o) <- "wflow_start"

  return(o)
}

#' @export
print.wflow_start <- function(x, ...) {
  if (x$dry_run) {
    cat("wflow_start (\"dry run mode\"):\n")
    if (x$existing) {
      cat(sprintf("- Files would be added to existing directory %s\n", x$directory))
    } else {
      cat(sprintf("- New directory would be created at %s\n", x$directory))
    }
    cat(sprintf("- Project name would be \"%s\"\n", x$name))
    if (x$change_wd) {
      cat(sprintf("- Working directory would be changed to %s\n", x$directory))
    } else {
      cat(sprintf("- Working directory would continue to be %s\n", getwd()))
    }
    if (x$existing && git2r::in_repository(x$directory)) {
      repo <- git2r::repository(x$directory, discover = TRUE)
      cat(sprintf("- Git repo already present at %s\n", repo@path))
    } else if (x$git) {
      cat(sprintf("- Git repo would be initiated at %s\n", x$directory))
    } else {
      cat(sprintf("- Git repo would not be initiated\n", x$directory))
    }
    if (x$git) {
      cat("- Files would be commited with Git\n")
    } else {
      cat("- Files would not be commited with Git\n")
    }
  } else {
    cat("wflow_start:\n")
    if (x$existing) {
      cat(sprintf("- File added to existing directory %s\n", x$directory))
    } else {
      cat(sprintf("- New directory created at %s\n", x$directory))
    }
    cat(sprintf("- Project name is \"%s\"\n", x$name))
    if (x$change_wd) {
      cat(sprintf("- Working directory changed to %s\n", x$directory))
    } else {
      cat(sprintf("- Working directory continues to be %s\n", getwd()))
    }
    if (git2r::in_repository(x$directory)) {
      repo <- git2r::repository(x$directory, discover = TRUE)
      if (x$git && !x$existing) {
        cat(sprintf("- Git repo inititated at %s\n", repo@path))
      } else if (x$git && x$existing && length(git2r::commits(repo)) == 1) {
        cat(sprintf("- Git repo inititated at %s\n", repo@path))
      } else {
        cat(sprintf("- Git repo already present at %s\n", repo@path))
      }
      if (x$git) {
        if (is.null(x$commit)) {
          cat("- Files were not committed\n")
        } else {
          cat(sprintf("- Files were committed in version %s\n",
                      shorten_sha(x$commit@sha)))
        }
      }
    } else {
      cat("- No Git repo\n")
    }
  }

  return(invisible(x))
}

check_rstudio_version <- function() {
  if (rstudioapi::isAvailable()) {
    rs_version <- rstudioapi::getVersion()
    if (rs_version < "1.0.0") {
      message(strwrap(paste("You can gain lots of new useful features",
                        "by updating to RStudio version 1.0 or greater.",
                        "You are running RStudio",
                        as.character(rs_version)), prefix = "\n"))
    }
  } else {
    rs_version <- NULL
  }
  return(rs_version)
}

# Check for user.name and user.email in .gitconfig
#
# path character. Path to repository
#
# If unable to find user.name and user.email, stops the program.
check_git_config <- function(path) {
  stopifnot(is.character(path))
  # Only look for local configuration file if the directory exists and it is a
  # Git repo
  if (dir.exists(path)) {
    look_for_local <- git2r::in_repository(path)
  } else {
    look_for_local <- FALSE
  }

  # Determine if user.name and user.email are set
  if (look_for_local) {
    r <- git2r::repository(path)
    git_config <- git2r::config(r)
    config_email_set <- "user.email" %in% names(git_config$global) |
                        "user.email" %in% names(git_config$local)
    config_name_set <- "user.name" %in% names(git_config$global) |
                       "user.name" %in% names(git_config$local)
  } else {
    git_config <- git2r::config()
    config_email_set <- "user.email" %in% names(git_config$global)
    config_name_set <- "user.name" %in% names(git_config$global)
  }

  if (config_email_set & config_name_set) {
    return(invisible())
  } else {
   stop("You must set your user.name and user.email for Git first\n",
        "to be able to run `wflow_start` with `git = TRUE`.\n",
        "Run the following command in R, replacing the arguments\n",
        "with your name and email address, and then re-run `wflow_start`:\n",
        "\n",
        'wflow_git_config(user.name = "Your Name", user.email = "email@domain")',
        call. = FALSE)
  }
}
