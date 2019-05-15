# It would be nice if wflow_use_github() could also create the repository on
# GitHub. Requiring the user to create and configure a Personal Access Token
# isn't worth it since this is only done once per project. Ideally the user
# could simply authenticate in the browser.

# https://github.com/r-lib/httr/blob/4d6eca9146a88cda20e7bee9dca125345d594885/demo/oauth2-github.r
# https://developer.github.com/apps/building-oauth-apps/authorizing-oauth-apps/
# https://cran.r-project.org/web/packages/googlesheets/vignettes/managing-auth-tokens.html
# https://developer.github.com/apps/building-oauth-apps/understanding-scopes-for-oauth-apps/
# https://developer.github.com/v3/#parameters

username <- "jdblischak"
repository <- "test-httr"

testing <- httr::oauth_app("github",
                           key = "274d4ff47ea4ed91d66a",
                           secret = "20261c8f17c6876bec2ad890f1aeadf0e1646dc3")

github_token <- httr::oauth2.0_token(httr::oauth_endpoints("github"), testing,
                                     scope = c("public_repo"),
                                     cache = FALSE)
gtoken <- httr::config(token = github_token)

req <- httr::GET("https://api.github.com/rate_limit", gtoken)
httr::stop_for_status(req)
httr::content(req)

# Confirm the repository doesn't exist
req <- httr::GET(glue::glue("https://api.github.com/repos/{username}/{repository}"), gtoken)
httr::stop_for_status(req)
httr::content(req)

# Create the repository
gh::gh("POST /user/repos", name = repository)

# Confirm the repository exists
req <- httr::GET(glue::glue("https://api.github.com/repos/{username}/{repository}"), gtoken)
httr::stop_for_status(req)
httr::content(req)
