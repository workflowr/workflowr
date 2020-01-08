#' Deploy site with GitLab
#'
#' \code{wflow_use_gitlab} automates all the local configuration necessary to
#' deploy your workflowr project with
#' \href{https://docs.gitlab.com/ee/ci/yaml/README.html#pages}{GitLab Pages}.
#' Afterwards, you will need to run \code{wflow_git_push} in the R console (or
#' \code{git push} in the terminal) to push the code to GitLab. Note that this
#' will also create the repository if it doesn't exist yet (this requires GitLab
#' 10.5 or greater). Alternatively, you could manually login to your account and
#' create the new repository on GitLab prior to pushing.
#'
#' \code{wflow_use_gitlab} performs the following steps and then commits the
#' changes:
#'
#' \itemize{
#'
#' \item Renames the website directory from \code{docs/} to \code{public/}
#'
#' \item Edits the setting \code{output_dir} in the file \code{_site.yml} to
#' save the website files in \code{public/}
#'
#' \item Adds a link to the GitLab repository in the navigation bar
#'
#' \item Creates the required file \code{.gitlab-ci.yml}
#'
#' \item Configures the Git remote settings to use GitLab
#'
#' }
#'
#' For more details, read the documentation provided by
#' \href{https://docs.gitlab.com/ee/ci/yaml/README.html#pages}{GitLab Pages}.
#'
#' @param username character (default: NULL). The GitLab account associated with
#'   the GitLab repository. This is likely your personal GitLab username, but it
#'   could also be the name of a GitLab organization you belong to. It will be
#'   combined with the arguments \code{repository} and \code{domain} to
#'   determine the URL of the new repository, e.g. the default is
#'   https://gitlab.com/username/repository. It will be combined with the
#'   arguments \code{repository}, \code{domain}, and \code{protocol} to
#'   determine the URL for Git to use to push and pull from GitLab, e.g. the
#'   default is https://gitlab.com/username/repository.git. If \code{username}
#'   is not specified, \code{wflow_use_gitlab} will first attempt to guess it
#'   from the current setting for the remote URL named "origin". If you haven't
#'   previously configured a remote for this workflowr project (or you are
#'   unsure what that means), then you should specify your GitLab username when
#'   calling this function.
#' @param repository character (default: NULL). The name of the remote
#'   repository on GitLab. If not specified, workflowr will guess the name of
#'   the repository. First, it will check the current setting for the remote URL
#'   named "origin". Second, it will use the name of the root directory of the
#'   workflowr project.
#' @param navbar_link logical (default: TRUE). Insert a link to the GitLab
#'   repository into the navigation bar.
#' @param protocol character (default: "https"). The protocol for communicating
#'   with GitLab. Must be either "https" or "ssh".
#' @param domain character (default: "gitlab.com"). The domain of the remote
#'   host. You only need to change this if you are using a custom GitLab
#'   instance hosted by your organization. For example, "git.rcc.uchicago.edu"
#'   is the domain for the GitLab instance hosted by the University of Chicago
#'   Research Computing Center.
#' @param project character (default: ".") By default the function assumes the
#'   current working directory is within the project. If this is not true,
#'   you'll need to provide the path to the project directory.
#'
#' @return Invisibly returns a list of class \code{wflow_use_gitlab}. This is
#'   currently for internal use only. Please open an Issue if you'd like to use
#'   this information.
#'
#' @seealso \code{\link{wflow_git_push}}, \code{\link{wflow_git_remote}},
#'          \code{\link{wflow_use_github}}
#'
#' @examples
#' \dontrun{
#'
#' wflow_use_gitlab("your-username", "name-of-repository")
#' # Login with GitLab account and create new repository
#' wflow_git_push()
#' }
#'
#'@export
wflow_use_gitlab <- function(username = NULL, repository = NULL,
                             navbar_link = TRUE,
                             protocol = "https",
                             domain = "gitlab.com",
                             project = ".") {

  # Check input arguments ------------------------------------------------------

  if (!is.null(username))
    if (!(is.character(username) && length(username) == 1))
      stop("username must be NULL or a one element character vector: ", username)

  if (!is.null(repository))
    if (!(is.character(repository) && length(repository) == 1))
      stop("repository must be NULL or a one element character vector: ", repository)

  if (!(is.logical(navbar_link) && length(navbar_link) == 1))
    stop("navbar_link must be a one-element logical vector")

  if (!(is.character(project) && length(project) == 1))
    stop("project must be a one-element character vector")

  check_wd_exists()

  if (!fs::dir_exists(project)) {
    stop("project directory does not exist.")
  }

  project <- absolute(project)

  # Status ---------------------------------------------------------------------

  s <- wflow_status(project = project)
  # Convert to absolute paths to facilitate path manipulation below
  s$analysis <- absolute(s$analysis)
  s$docs <- absolute(s$docs)

  r <- git2r::repository(path = s$git)
  remotes <- wflow_git_remote(verbose = FALSE, project = project)

  message("Summary from wflow_use_gitlab():")

  # Determine username and repository ------------------------------------------

  # guess based on current remote "origin"
  host <- get_host_from_remote(path = project) # returns NA if unavailable
  host_parts <- stringr::str_split(host, "/")[[1]]

  if (is.null(username)) {
    if (is.na(host)) {
      stop("Unable to guess username. Please specify this argument.")
    } else {
      username <- host_parts[length(host_parts) - 1]
    }
  }
  message("username: ", username)

  if (is.null(repository)) {
    if (is.na(host)) {
      # Use root directory name
      repository <- fs::path_file(absolute(s$root))
    } else {
      repository <- host_parts[length(host_parts)]
    }
  }
  message("respository: ", repository)

  # Rename docs/ to public/ ----------------------------------------------------

  if (basename(s$docs) == "public") {
    message("* The website directory is already named public/")
    renamed <- NA
  } else {
    public <- file.path(dirname(s$docs), "public")
    renamed <- wflow_rename(s$docs, public, git = FALSE, project = project)
    git2r_add(r, renamed$files_git)
    message("* Created the website directory public/")
  }

  # Edit output_dir in _site.yml -----------------------------------------------

  site_yml_fname <- file.path(s$analysis, "_site.yml")
  if (!fs::file_exists(site_yml_fname)) {
    stop("The website configuration file _site.yml does not exist.")
  }
  site_yml <- yaml::yaml.load_file(site_yml_fname)
  if (site_yml$output_dir == "../public") {
    message("* Output directory is already set to public/")
  } else {
    site_yml$output_dir <- "../public"
    yaml::write_yaml(site_yml, file = site_yml_fname)
    git2r_add(r, site_yml_fname)
    message("* Set output directory to public/")
  }

  # .gitlab-ci.yml -------------------------------------------------------------

  # The list `gitlab` is defined in R/infrastructure.R
  gitlab_yml <- gitlab[[".gitlab-ci.yml"]]
  gitlab_yml_fname <- file.path(s$root, ".gitlab-ci.yml")
  if (fs::file_exists(gitlab_yml_fname)) {
    message("* .gitlab-ci.yml file already exists")
  } else {
    cat(glue::glue(gitlab_yml), file = gitlab_yml_fname)
    git2r_add(r, gitlab_yml_fname)
    message("* Created the file .gitlab-ci.yml")
  }

  # Configure Git remote -------------------------------------------------------

  # 3 possible scenarios:
  #   1. Remote is already set correctly -> Do nothing
  #   2. Remote "origin" is currently defined -> Update URL with set_url
  #   3. Remote "origin" does not exist -> Add remote "origin"
  url_anticipated <- create_remote_url(user = username, repo = repository,
                                       protocol = protocol, domain = domain)
  url_current <- remotes["origin"]
  if (!is.na(url_current) && url_current == url_anticipated) {
    config_remote <- NA
    message("* Remote \"origin\" already set to ", remotes["origin"])
  } else if ("origin" %in% names(remotes)) {
    config_remote <- wflow_git_remote(remote = "origin", user = username,
                                      repo = repository, protocol = protocol,
                                      action = "set_url", domain = domain,
                                      verbose = FALSE, project = project)
    message("* Changed remote \"origin\" to ", config_remote["origin"])
  } else {
    config_remote <- wflow_git_remote(remote = "origin", user = username,
                                      repo = repository, protocol = protocol,
                                      action = "add", domain = domain,
                                      verbose = FALSE, project = project)
    message("* Set remote \"origin\" to ", config_remote["origin"])
  }

  # Add link in navigation bar -------------------------------------------------

  host <- get_host_from_remote(path = project)
  if (navbar_link && !is.na(host)) {
    site_yml$navbar$right <- list(list(icon = "fa-gitlab",
                                       text = "Source code",
                                       href = host))
    yaml::write_yaml(site_yml, file = site_yml_fname)
    git2r_add(r, site_yml_fname)
    message("* Added GitLab link to navigation bar")
  }

  # Commit changes -------------------------------------------------------------

  # Obtain staged files
  files_git <- git2r::status(r, staged = TRUE, unstaged = FALSE, untracked = FALSE)
  files_git <- unlist(files_git$staged)
  names(files_git) <- NULL
  if (length(files_git) > 0) {
    commit <- git2r::commit(r, message = "Host with GitLab.")
    message("* Committed the changes to Git")
  } else {
    commit <- NA
  }


  # Prepare output -------------------------------------------------------------

  o <- list(username = username, repository = repository,
            renamed = renamed, files_git = files_git, commit = commit,
            config_remote = config_remote)
  class(o) <- "wflow_use_gitlab"

  message("To do: Run wflow_git_push() to send your project to GitLab")
  message("Note: The push will create the new repository if it doesn't exist yet")

  return(invisible(o))
}
