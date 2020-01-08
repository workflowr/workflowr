#' Deploy site with GitHub
#'
#' \code{wflow_use_github} automates all the local configuration necessary to
#' deploy your workflowr project with \href{https://pages.github.com/}{GitHub
#' Pages}. Optionally, it can also create the new repository on GitHub (only
#' applies to public repositories hosted on github.com). Afterwards, you will
#' need to run \code{wflow_git_push} in the R console (or \code{git push} in the
#' terminal) to push the code to GitHub.
#'
#' \code{wflow_use_github} performs the following steps and then commits the
#' changes:
#'
#' \itemize{
#'
#' \item Adds a link to the GitHub repository in the navigation bar
#'
#' \item Configures the Git remote settings to use GitHub (via
#' \code{\link{wflow_git_remote}})
#'
#' \item (Only if necessary) Renames the website directory to \code{docs/}
#'
#' \item (Only if necessary) Edits the setting \code{output_dir} in the file
#' \code{_site.yml} to save the website files in \code{docs/}
#'
#' }
#'
#' Furthermore, you have two options for creating the remote repository on GitHub.
#' In an interactive R session, you will be prompted to choose one of the options
#' below. To bypass the prompt, you can set the argument \code{create_on_github}.
#'
#' \itemize{
#'
#' \item 1. Have workflowr create the new repository on GitHub. If you accept, your
#' browser will open for you to provide authorization. If you are not logged
#' into GitHub, you will be prompted to login. Then you will be asked to give
#' permission to the workflowr-oauth-app to create the new repository for you on
#' your behalf. This will allow \code{wflow_use_github}, running on your own
#' machine, to create your new repository. Once \code{wflow_use_github}
#' finishes, workflowr can no longer access your GitHub account.
#'
#' \item 2. Create the remote repository yourself by going to
#' \url{https://github.com/new} and entering the Repository name that matches
#' the name of the directory of your workflowr project (if you used the argument
#' \code{repository} to make it a different name, make sure to instead use that
#' one).
#'
#' }
#'
#' Once the GitHub repository has been created either by \code{wflow_use_github}
#' or yourself, run \code{wflow_git_push} in the R console (or \code{git push
#' origin master} in the terminal) to push your code to GitHub.
#'
#' @param username character (default: NULL). The GitHub account associated with
#'   the GitHub repository. This is likely your personal GitHub username, but it
#'   could also be the name of a GitHub organization you belong to. It will be
#'   combined with the arguments \code{repository} and \code{domain} to
#'   determine the URL of the new repository, e.g. the default is
#'   https://github.com/username/repository. It will be combined with the
#'   arguments \code{repository}, \code{domain}, and \code{protocol} to
#'   determine the URL for Git to use to push and pull from GitHub, e.g. the
#'   default is https://github.com/username/repository.git. If \code{username}
#'   is not specified, \code{wflow_use_github} will first attempt to guess it
#'   from the current setting for the remote URL named "origin". If you haven't
#'   previously configured a remote for this workflowr project (or you are
#'   unsure what that means), then you should specify your GitHub username when
#'   calling this function.
#' @param repository character (default: NULL). The name of the remote
#'   repository on GitHub. If not specified, workflowr will guess the name of
#'   the repository. First, it will check the current setting for the remote URL
#'   named "origin". Second, it will use the name of the root directory of the
#'   workflowr project.
#' @param navbar_link logical (default: TRUE). Insert a link to the GitHub
#'   repository into the navigation bar.
#' @param create_on_github logical (default: NULL). Should workflowr create the
#'   repository on GitHub? This requires logging into your GitHub account to
#'   authenticate workflowr to act on your behalf. The default behavior is to
#'   ask the user. Note that this only works for public repositories on
#'   github.com. If you want to create a private repository or are using GitHub
#'   Enterprise, you will need to manually create the repository.
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
#' @section  Troubleshooting:
#'
#' The feature to automatically create the GitHub repository for you may fail
#' since it involves using your web browser to authenticate with your GitHub
#' account. If it fails for any reason, it'd probably be easier to manually
#' login to GitHub and create the repository yourself
#' (\href{https://help.github.com/articles/creating-a-new-repository/}{instructions from GitHub}).
#' However, if you have time, please file an
#' \href{https://github.com/jdblischak/workflowr/issues/new/choose}{Issue on
#' GitHub} to report what happened, and importantly include which web browser
#' you were using.
#'
#' We have observed the following problems before:
#'
#' \itemize{
#'
#' \item The green button to approve the authentication of the workflowr GitHub
#' app to create the repository on your behalf is grayed out, and unable to be
#' clicked. This is likely a JavaScript problem. Make sure you don't have
#' JavaScript disabled in your web browser. Also, you can try using a different
#' browser.
#'
#' }
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
#' @importFrom httpuv startServer
#' @export
wflow_use_github <- function(username = NULL, repository = NULL,
                             navbar_link = TRUE,
                             create_on_github = NULL,
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

  if (!is.null(create_on_github))
    if (!(is.logical(create_on_github) && length(create_on_github) == 1))
      stop("create_on_github must be NULL or a one element character vector: ", create_on_github)

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

  message("Summary from wflow_use_github():")

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
    message("* Overwrote previous remote \"origin\" to ", config_remote["origin"])
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

  # Create GitHub repository ---------------------------------------------------

  repo_created <- FALSE

  # Do not create repo if the domain is not github.com
  if (domain != "github.com") {
    if (isTRUE(create_on_github))
      warning("workflowr can only create a repository on github.com",
              call. = FALSE, immediate. = TRUE)
    create_on_github <- FALSE
  }

  if (is.null(create_on_github) && interactive()) {
    cat("\nTo proceed, you have two options:\n")

    cat("\n", wrap(glue::glue(
      "1. Have workflowr attempt to automatically create the repository \"{repository}\" on GitHub.
      This requires
      logging into GitHub and enabling the workflowr-oauth-app access to the
      account \"{username}\"."
    )), "\n", sep = "")

    cat("\n", wrap(glue::glue(
      "2. Create the repository \"{repository}\" yourself by going to https://github.com/new and entering \"{repository}\" for the Repository name. This is the default option."
    )), "\n", sep = "")

    ans <- readline("\nEnter your choice (1 or 2): ")
    if (ans == "1") {
      create_on_github <- TRUE
      cat("You chose option 1: have workflowr attempt to create repo\n")
    } else if (ans == "2") {
      cat("You chose option 2: create the repo yourself\n")
    } else {
      cat("Invalid input. Defaulting to option 2: create the repo yourself\n")
    }
  }

  if (is.null(create_on_github)) create_on_github <- FALSE

  if (create_on_github) {
    repo_url <- create_gh_repo(username, repository)
    if (check_browser()) utils::browseURL(repo_url)
    repo_created <- TRUE
    message(glue::glue("*  Created {username}/{repository}"))
  }

  # Prepare output -------------------------------------------------------------

  o <- list(username = username, repository = repository,
            renamed = renamed, files_git = files_git, commit = commit,
            config_remote = config_remote, repo_created = repo_created)
  class(o) <- "wflow_use_github"

  if (!repo_created) {
    message(glue::glue("To do: Create {username}/{repository} at {domain} (if it doesn't already exist)"))
  }

  message("To do: Run wflow_git_push() to push your project to GitHub")

  return(invisible(o))
}

# Create GitHub repository
create_gh_repo <- function(username, repository) {

  # Authenticate with GitHub
  app <- httr::oauth_app("github",
                         key = "341566cfd0c8017ba5ac",
                         secret = "ac5e6d52e3bf71e4535149622f053b9f00f2e155")

  # Set user agent
  ua <- httr::user_agent("https://github.com/jdblischak/workflowr")

  message(glue::glue(
    "Requesting authorization for workflowr app to access GitHub account {username}"))
  oauth_token <- httr::oauth2.0_token(httr::oauth_endpoints("github"),
                                      app,
                                      scope = c("public_repo"),
                                      cache = FALSE)
  token <- httr::config(token =  oauth_token)

  # Ensure they haven't exceeded their rate limit
  req_rate <- httr::GET("https://api.github.com/rate_limit", token, ua)
  httr::stop_for_status(req_rate)
  content_rate <- httr::content(req_rate)
  if (content_rate$resources$core$remaining < 5) {
    warning("You've exceeded your rate limit for the GitHub API.",
            " Please try again later.")
    return(NULL)
  }

  # Confirm the repository doesn't exist
  req_exist <- httr::GET(glue::glue("https://api.github.com/repos/{username}/{repository}"),
                         token, ua)
  status_exist <- httr::http_status(req_exist)
  if (status_exist$reason != "Not Found") {
    warning(glue::glue("Repository {repository} already exists for user {username}"),
            call. = FALSE, immediate. = TRUE)
    return(glue::glue("https://github.com/{username}/{repository}"))
  }

  # Create the repository
  message(glue::glue("Creating repository {repository}"))
  req_create <- httr::POST("https://api.github.com/user/repos", token, ua,
                           body = list(name = repository), encode = "json")
  httr::stop_for_status(req_create)

  # Confirm the repository exists
  req_confirm <- httr::GET(glue::glue("https://api.github.com/repos/{username}/{repository}"),
                           token, ua)
  status_confirm <- httr::http_status(req_confirm)
  if (status_confirm$category != "Success") {
    warning(glue::glue("Failed to create repository {repository}. Reason: {status_confirm$reason}"))
    return(NULL)
  }

  # Return the full URL to new repository
  content_confirm <- httr::content(req_confirm)
  return(content_confirm$html_url)
}
