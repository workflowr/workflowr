#' Deploy site with GitHub
#'
#' \code{wflow_use_github} automates all the local configuration necessary to
#' deploy your workflowr project with \href{https://pages.github.com/}{GitHub
#' Pages}. However, you will need to manually login to your account and create
#' the new repository on GitHub. The final step is to run \code{wflow_git_push}
#' in the R console.
#'
#' \code{wflow_use_github} performs the following steps and then commits the
#' changes:
#'
#' \itemize{
#'
#' \item Adds a link to the GitHub repository in the navigation bar
#'
#' \item Configures the Git remote settings to use GitHub
#'
#' \item (Only if necessary) Renames the website directory to \code{docs/}
#'
#' \item (Only if necessary) Edits the setting \code{output_dir} in the file
#' \code{_site.yml} to save the website files in \code{docs/}
#'
#' }
#'
#' For more details, read the documentation provided by
#' \href{https://pages.github.com/}{GitHub Pages}.
#'
#' @param username character (default: NULL). The GitHub username for the remote
#'   repository. If not specified, workflowr will attempt to guess this from the
#'   current remote named "origin" if it had previously been configured.
#' @param repository character (default: NULL). The name of the remote
#'   repository on GitHub. If not specified, workflowr will attempt to guess
#'   this from the current remote named "origin" if it had previously been
#'   configured.
#' @param navbar_link logical (default: TRUE). Insert a link to the GitHub
#'   repository into the navigation bar.
#' @param protocol character (default: "https"). The protocol for communicating
#'   with GitHub. Must be either "https" or "ssh".
#' @param domain character (default: "github.com"). The domain of the remote
#'   host. You only need to change this if your organization is using GitHub
#'   Enterprise.
#' @param project character (default: ".") By default the function assumes the
#'   current working directory is within the project. If this is not true,
#'   you'll need to provide the path to the project directory.
#'
#' @return Invisibly returns a list of class \code{wflow_use_github}. This is
#'   currently for internal use only. Please open an Issue if you'd like to use
#'   this information.
#'
#' @seealso \code{\link{wflow_git_push}}, \code{\link{wflow_git_remote}},
#'          \code{\link{wflow_use_gitlab}}
#'
#' @examples
#' \dontrun{
#'
#' wflow_use_github("your-username", "name-of-repository")
#' # Login with GitHub account and create new repository
#' wflow_git_push()
#' }
#'
#'@export
wflow_use_github <- function(username = NULL, repository = NULL,
                             navbar_link = TRUE,
                             protocol = "https",
                             domain = "github.com",
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

  if (!fs::dir_exists(project)) {
    stop("project directory does not exist.")
  }

  project <- absolute(project)

  # If username and/or repository are NULL, make sure that it can be guessed
  # from current remote "origin"
  host <- get_host_from_remote(path = project)
  if (is.null(username) || is.null(repository)) {
    if (is.na(host)) {
      stop("You must specify the arguments username and repository.")
    } else {
      host_parts <- stringr::str_split(host, "/")[[1]]
      username <- host_parts[length(host_parts) - 1]
      repository <- host_parts[length(host_parts)]
      message("username: ", username)
      message("respository: ", repository)
    }
  }

  message("Summary from wflow_use_github():")

  # Status ---------------------------------------------------------------------

  s <- wflow_status(project = project)
  # Convert to absolute paths to facilitate path manipulation below
  s$analysis <- absolute(s$analysis)
  s$docs <- absolute(s$docs)

  r <- git2r::repository(path = s$git)
  remotes <- wflow_git_remote(verbose = FALSE, project = project)

  # Rename docs/ to public/ ----------------------------------------------------

  if (basename(s$docs) == "docs") {
    message("* The website directory is already named docs/")
    renamed <- NA
  } else {
    docs <- file.path(dirname(s$docs), "docs")
    renamed <- wflow_rename(s$docs, docs, git = FALSE, project = project)
    git2r_add(r, renamed$files_git)
    message("* Created the website directory docs/")
  }

  # Edit output_dir in _site.yml -----------------------------------------------

  site_yml_fname <- file.path(s$analysis, "_site.yml")
  if (!fs::file_exists(site_yml_fname)) {
    stop("The website configuration file _site.yml does not exist.")
  }
  site_yml <- yaml::yaml.load_file(site_yml_fname)
  if (site_yml$output_dir == "../docs") {
    message("* Output directory is already set to docs/")
  } else {
    site_yml$output_dir <- "../docs"
    yaml::write_yaml(site_yml, file = site_yml_fname)
    git2r_add(r, site_yml_fname)
    message("* Set output directory to docs/")
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
    site_yml$navbar$right <- list(list(icon = "fa-github",
                                       text = "Source code",
                                       href = host))
    yaml::write_yaml(site_yml, file = site_yml_fname)
    git2r_add(r, site_yml_fname)
    message("* Added GitHub link to navigation bar")
  }

  # Commit changes -------------------------------------------------------------

  # Obtain staged files
  files_git <- git2r::status(r, staged = TRUE, unstaged = FALSE, untracked = FALSE)
  files_git <- unlist(files_git$staged)
  names(files_git) <- NULL
  if (length(files_git) > 0) {
    commit <- git2r::commit(r, message = "Host with GitHub.")
    message("* Committed the changes to Git")
  } else {
    commit <- NA
  }

  # Prepare output -------------------------------------------------------------

  o <- list(renamed = renamed, files_git = files_git, commit = commit,
            config_remote = config_remote)
  class(o) <- "wflow_use_github"

  message("\nGitHub configuration successful!\n")
  message("To do: Create new repository at ", domain)
  message("To do: Run wflow_git_push() to send your project to GitHub")

  return(invisible(o))
}

# Create GitHub repository
create_gh_repo <- function(username, repository) {

  # Authenticate with GitHub
  app <- httr::oauth_app("github",
                         key = "274d4ff47ea4ed91d66a",
                         secret = "20261c8f17c6876bec2ad890f1aeadf0e1646dc3")

  oauth_token <- httr::oauth2.0_token(httr::oauth_endpoints("github"),
                                      app,
                                      scope = c("public_repo"),
                                      cache = FALSE)
  token <- httr::config(token =  oauth_token)

  # Ensure they haven't exceeded their rate limit
  req_rate <- httr::GET("https://api.github.com/rate_limit", token)
  httr::stop_for_status(req_rate)
  content_rate <- httr::content(req_rate)
  if (content_rate$resources$core$remaining < 5) {
    warning("You've exceeded your rate limit for the GitHub API.",
            " Please try again later.")
    return(NULL)
  }

  # Confirm the repository doesn't exist
  req_exist <- httr::GET(glue::glue("https://api.github.com/repos/{username}/{repository}"),
                         token)
  status_exist <- httr::http_status(req_exist)
  if (status_exist$reason != "Not Found") {
    warning(glue::glue("Repository {repository} already exists for user {username}"))
    return(NULL)
  }

  # Create the repository
  req_create <- httr::POST("https://api.github.com/user/repos", token,
                           body = list(name = repository), encode = "json")
  httr::stop_for_status(req_create)

  # Confirm the repository exists
  req_confirm <- httr::GET(glue::glue("https://api.github.com/repos/{username}/{repository}"),
                           token)
  status_confirm <- httr::http_status(req_confirm)
  if (status_confirm$category != "Success") {
    warning(glue::glue("Failed to create repository {repository}. Reason: {status_confirm$reason}"))
    return(NULL)
  }

  # Return the full URL to new repository
  content_confirm <- httr::content(req_confirm)
  return(content_confirm$html_url)
}
