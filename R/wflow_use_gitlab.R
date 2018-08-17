#' wflow_use_gitlab
#'
#' Automate setup for deploying website with GitLab:
#'
#' 1. Rename docs/ to public/
#' 2. Edit output_dir in _site.yml
#' 3. Create .gitlab-ci.yml
#'
#' https://docs.gitlab.com/ee/ci/yaml/README.html#pages
#'
#'@export
wflow_use_gitlab <- function(project = ".") {

  # Status ---------------------------------------------------------------------

  s <- wflow_status(project = project)
  r <- git2r::repository(path = s$git)

  # Rename docs/ to public/ ----------------------------------------------------

  if (basename(s$docs) == "public") {
    message("The website directory is already named \"public\"")
  } else {
    public <- file.path(dirname(s$docs), "public")
    file.rename(s$docs, public)
    git2r::add(r, public)
    git2r::add(r, s$docs)
  }

  # wflow_rename()

  # Edit output_dir in _site.yml -----------------------------------------------

  site_yml_fname <- file.path(s$analysis, "_site.yml")
  if (!file.exists(site_yml_fname)) {
    stop("The website configuration file _site.yml does not exist.")
  }
  site_yml <- yaml::yaml.load_file(site_yml_fname)
  if (site_yml$output_dir == "../public") {
    message("Output directory is already set to \"../public\"")
  } else {
    # yaml::write_yaml creates a lot of noise (it removes quotation marks),
    # so instead directly edit text.
    site_yml_text <- readLines(site_yml_fname)
    site_yml_gitlab <- stringr::str_replace(site_yml_text,
                                            "output_dir: .+",
                                            "output_dir: \"../public\"")
    writeLines(site_yml_gitlab, con = site_yml_fname)
    git2r::add(r, site_yml_fname)
  }

  # .gitlab-ci.yml -------------------------------------------------------------

  # The list `gitlab` is defined in R/infrastructure.R
  gitlab_yml <- gitlab[[".gitlab-ci.yml"]]
  gitlab_yml_fname <- file.path(s$root, ".gitlab-ci.yml")
  if (file.exists(gitlab_yml_fname)) {
    message(".gitlab-ci.yml file already exists")
  } else {
    cat(glue::glue(gitlab_yml), file = gitlab_yml_fname)
    git2r::add(r, gitlab_yml_fname)
  }

  # Commit changes -------------------------------------------------------------

  git2r::commit(r, message = "Host with GitLab.")

  return()
}
