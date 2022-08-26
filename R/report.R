
create_report <- function(input, output_dir, has_code, opts) {
  if (opts$suppress_report) {
    return("")
  }

  input <- absolute(input)
  input_dir <- dirname(input)

  uses_git <- git2r::in_repository(input_dir)
  if (uses_git) {
    r <- git2r::repository(input_dir, discover = TRUE)
    s <- git2r::status(r, ignored = TRUE)
  } else {
    r <- NULL
    s <- NULL
  }

  # workflowr checks ------------------------------------------------------
  checks <- list()

  # Check R Markdown status
  if (uses_git) {
    checks$result_rmd <- check_rmd(input, r, s)
  }

  if (has_code) {
    # Check environment
    checks$result_environment <- check_environment()

    # Check seed
    checks$result_seed <- check_seed(opts$seed)

    # Check sessioninfo
    checks$result_sessioninfo <- check_sessioninfo(input, opts$sessioninfo)

    # Check caching
    checks$cache <- check_cache(input)

    # Check for absolute paths
    checks$paths <- check_paths(input, opts$knit_root_dir)
  }

  # Check version control
  checks$result_vc <- check_vc(input, r, s, opts$github, output_dir = output_dir)

  # Formatting checks -----------------------------------------------------

  checks_formatted <- Map(format_check, checks)
  checks_formatted_string <- paste(unlist(checks_formatted), collapse = "\n")
  report_checks <- glue::glue('
    <div class="panel-group" id="workflowr-checks">
      {checks_formatted_string}
    </div>
    ')

  # Format `knit_root_dir` for display in report.
  knit_root_print <- opts$knit_root_dir
  # If it is part of a workflowr project, construct a path relative to the
  # directory that contains the workflowr project directory.
  p <- try(wflow_paths(error_git = FALSE, project = input_dir), silent = TRUE)
  if (!inherits(p, "try-error")) {
    if (fs::path_has_parent(knit_root_print, absolute(p$root))) {
      knit_root_print <- fs::path_rel(knit_root_print,
                                      start = dirname(absolute(p$root)))
    }
  } else {
    # Otherwise, just replace the home directory with ~
    knit_root_print <- stringr::str_replace(knit_root_print,
                                            fs::path_home(),
                                            "~")
  }
  # Add trailing slash
  if (!stringr::str_detect(knit_root_print, "/$")) {
    knit_root_print <- paste0(knit_root_print, "/")
  }

  # Version history --------------------------------------------------------

  if (uses_git) {
    versions <- get_versions(input, output_dir, r, opts$github)
    report_versions <- versions
  } else {
    report_versions <-
      "<p>This project is not being versioned with Git. To obtain the full
        reproducibility benefits of using workflowr, please see
        <code>?wflow_start</code>.</p>"
  }

  # Return -----------------------------------------------------------------

  checks_passed <- vapply(checks, function(x) x$pass, FUN.VALUE = logical(1))
  if (all(checks_passed)) {
    symbol <- "glyphicon-ok text-success"
  } else {
    symbol <- "glyphicon-exclamation-sign text-danger"
  }
  report <- glue::glue('
    <p>
    <button type="button" class="btn btn-default btn-workflowr btn-workflowr-report"
      data-toggle="collapse" data-target="#workflowr-report">
      <span class="glyphicon glyphicon-list" aria-hidden="true"></span>
      workflowr
      <span class="glyphicon {symbol}" aria-hidden="true"></span>
    </button>
    </p>

    <div id="workflowr-report" class="collapse">
    <ul class="nav nav-tabs">
      <li class="active"><a data-toggle="tab" href="#summary">Summary</a></li>
      <li><a data-toggle="tab" href="#checks">
      Checks <span class="glyphicon {symbol}" aria-hidden="true"></span>
      </a></li>
      <li><a data-toggle="tab" href="#versions">Past versions</a></li>
    </ul>

    <div class="tab-content">
    <div id="summary" class="tab-pane fade in active">
      <p><strong>Last updated:</strong> {Sys.Date()}</p>
      <p><strong>Checks:</strong>
      <span class="glyphicon glyphicon-ok text-success" aria-hidden="true"></span>
      {sum(checks_passed)}
      <span class="glyphicon glyphicon-exclamation-sign text-danger" aria-hidden="true"></span>
      {sum(!checks_passed)}
      </p>
      <p><strong>Knit directory:</strong>
      <code>{knit_root_print}</code>
      <span class="glyphicon glyphicon-question-sign" aria-hidden="true"
      title="This is the local directory in which the code in this file was executed.">
      </span>
      </p>
      <p>
      This reproducible <a href="https://rmarkdown.rstudio.com">R Markdown</a>
      analysis was created with <a
      href="https://github.com/workflowr/workflowr">workflowr</a> (version
      {packageVersion("workflowr")}). The <em>Checks</em> tab describes the
      reproducibility checks that were applied when the results were created.
      The <em>Past versions</em> tab lists the development history.
      </p>
    <hr>
    </div>
    <div id="checks" class="tab-pane fade">
      {report_checks}
    <hr>
    </div>
    <div id="versions" class="tab-pane fade">
      {report_versions}
    <hr>
    </div>
    </div>
    </div>
    ')

  return(report)
}

get_versions <- function(input, output_dir, r, github) {

  rmd <- input
  html <- to_html(rmd, outdir = output_dir)

  df_versions <- get_versions_df(c(rmd, html), r)

  # Convert paths to be relative to Git root
  rmd <- relative(rmd, start = git2r::workdir(r))
  html <- relative(html, start = git2r::workdir(r))
  df_versions$File <- relative(df_versions$File, start = git2r::workdir(r))

  # Exit early if there are no past versions
  if (length(df_versions) == 0) {
    text <-
      "<p>There are no past versions. Publish this analysis with
      <code>wflow_publish()</code> to start tracking its development.</p>"
    return(text)
  }

  df_versions$File <- ifelse(df_versions$File == rmd, "Rmd", "html")

  if (is.na(github)) {
    df_versions$Version <- shorten_sha(df_versions$Version)
  } else {
    df_versions$Version <- ifelse(df_versions$File == "html",
                                  # HTML preview URL
                                  create_url_html(github, html, df_versions$Version),
                                  # R Markdown URL
                                  sprintf("<a href=\"%s/blob/%s/%s\" target=\"_blank\">%s</a>",
                                          github, df_versions$Version, rmd,
                                          shorten_sha(df_versions$Version)))
  }

  df_versions <- df_versions[, c("File", "Version", "Author", "Date", "Message")]

  template <-
"
<p>
These are the previous versions of the repository in which changes were made
to the R Markdown (<code>{{rmd}}</code>) and HTML (<code>{{html}}</code>)
files. If you've configured a remote Git repository (see
<code>?wflow_git_remote</code>), click on the hyperlinks in the table below to
view the files as they were in that past version.
</p>
<div class=\"table-responsive\">
<table class=\"table table-condensed table-hover\">
<thead>
<tr>
<th>File</th>
<th>Version</th>
<th>Author</th>
<th>Date</th>
<th>Message</th>
</tr>
</thead>
<tbody>
{{#df_versions}}
<tr>
<td>{{{File}}}</td>
<td>{{{Version}}}</td>
<td>{{Author}}</td>
<td>{{Date}}</td>
<td>{{Message}}</td>
</tr>
{{/df_versions}}
</tbody>
</table>
</div>
"
  data <- list(rmd = rmd, html = html,
               df_versions = unname(whisker::rowSplit(df_versions)))
  text <- whisker::whisker.render(template, data)

  return(text)
}

# Get versions table for figures. Needs to be refactored to share code with
# get_versions.
get_versions_fig <- function(fig, r, github) {
  df_versions <- get_versions_df(fig, r)

  fig <- relative(fig, start = git2r::workdir(r))

  # Exit early if there are no past versions
  if (length(df_versions) == 0) {
    return("")
  }

  if (is.na(github)) {
    df_versions$Version <- shorten_sha(df_versions$Version)
  } else {
    df_versions$Version <- sprintf("<a href=\"%s/blob/%s/%s\" target=\"_blank\">%s</a>",
                                   github, df_versions$Version, fig,
                                   shorten_sha(df_versions$Version))
  }

  df_versions <- df_versions[, c("Version", "Author", "Date")]

  fig <- basename(fig)
  id <- paste0("fig-", tools::file_path_sans_ext(basename(fig)))
  # An HTML ID cannot contain spaces. If filename has spaces, quote the figure
  # name and convert spaces in ID to dashes. Also insert text in case there is a
  # similar chunk name that already uses dashes instead of spaces.
  if (stringr::str_detect(fig, "\\s")) {
    fig <- paste0('"', fig, '"')
    id <- stringr::str_replace_all(id, "\\s", "-")
    id <- stringr::str_replace(id, "fig-", "fig-no-spaces-")
  }

  template <-
    "
  <p>
  <button type=\"button\" class=\"btn btn-default btn-xs btn-workflowr btn-workflowr-fig\"
  data-toggle=\"collapse\" data-target=\"#{{id}}\">
  Past versions of {{fig}}
  </button>
  </p>

  <div id=\"{{id}}\" class=\"collapse\">
  <div class=\"table-responsive\">
  <table class=\"table table-condensed table-hover\">
  <thead>
  <tr>
  <th>Version</th>
  <th>Author</th>
  <th>Date</th>
  </tr>
  </thead>
  <tbody>
  {{#df_versions}}
  <tr>
  <td>{{{Version}}}</td>
  <td>{{Author}}</td>
  <td>{{Date}}</td>
  </tr>
  {{/df_versions}}
  </tbody>
  </table>
  </div>
  </div>
  "
  data <- list(fig = fig, id = id,
               df_versions = unname(whisker::rowSplit(df_versions)))
  text <- whisker::whisker.render(template, data)

  return(text)

}

# Return a data frame of past versions
#
# files - paths to files
# r - git_repository
# timezone - timezone to use, e.g. "America/New_York". Defaults to local
#            timezone. If unset (i.e. is NULL, NA, or ""), defaults to "Etc/UTC".
#
# If no past versions, returns empty data frame
get_versions_df <- function(files, r, timezone = Sys.timezone()) {

  commits_path <- list()
  for (f in files) {
    commits_f <- git2r::commits(r, path = f)
    names(commits_f) <- rep(f, length(commits_f))
    commits_path <- c(commits_path, commits_f)
  }

  # Exit early if there are no past versions
  if (length(commits_path) == 0) {
    return(data.frame())
  }

  version <- vapply(commits_path, function(x) x$sha, character(1))
  author <- vapply(commits_path, function(x) x$author$name, character(1))
  date <- lapply(commits_path, function(x) as.POSIXct(x$author$when))
  date <- do.call(c, date)
  message <- vapply(commits_path, function(x) x$message, character(1))

  # Only keep the first line of the commit message
  message <- vapply(message, get_first_line, character(1))

  df_versions <- data.frame(File = names(commits_path), Version = version,
                            Author = author, Date = date,
                            Message = message, stringsAsFactors = FALSE)
  df_versions <- df_versions[order(df_versions$Date, decreasing = TRUE), ]
  if (is.null(timezone) || is.na(timezone) || identical(timezone, "")) {
    timezone <- "Etc/UTC"
  }
  df_versions$Date <- as.character(as.Date(df_versions$Date, tz = timezone))
  rownames(df_versions) <- seq_len(nrow(df_versions))

  return(df_versions)
}

check_vc <- function(input, r, s, github, output_dir) {
 if (!is.null(r)) {
   pass <- TRUE
   log <- git2r::commits(r)
   if (length(log) > 0) {
     sha <- log[[1]]$sha
     sha7 <- shorten_sha(sha)
     if (!is.na(github)) {
       sha_display <- sprintf("<a href=\"%s/tree/%s\" target=\"_blank\">%s</a>",
                              github, sha, sha7)
     } else {
       sha_display <- sha7
     }
   } else {
     sha_display <- "No commits yet"
   }
   summary <- sprintf("<strong>Repository version:</strong> %s", sha_display)
   # Scrub HTML and other generated content (e.g. site_libs). It's ok that these
   # have uncommitted changes.
   s <- scrub_status(s, r, output_dir = output_dir)

   status <- utils::capture.output(print(s))
   status <- c("<pre><code>", status, "</code></pre>")
   status <- paste(status, collapse = "\n")
   details <-
"
<p>
Great! You are using Git for version control. Tracking code development and
connecting the code version to the results is critical for reproducibility.
</p>
"
   if (sha_display != "No commits yet") {
     details <- c(details,
                  glue::glue(
"<p>
The results in this page were generated with repository version {sha_display}.
See the <em>Past versions</em> tab to see a history of the changes made to the
R Markdown and HTML files.
</p>"
                  ))
   }
   details <- c(details,
"
<p>
Note that you need to be careful to ensure that all relevant files for the
analysis have been committed to Git prior to generating the results (you can
use <code>wflow_publish</code> or <code>wflow_git_commit</code>). workflowr only
checks the R Markdown file, but you know if there are other scripts or data
files that it depends on. Below is the status of the Git repository when the
results were generated:
</p>
",
status,
"
<p>
Note that any generated files, e.g. HTML, png, CSS, etc., are not included in
this status report because it is ok for generated content to have uncommitted
changes.
</p>
")
   details <- paste(details, collapse = "\n")
 } else {
   pass <- FALSE
   summary <- "<strong>Repository version:</strong> no version control"
   details <-
"
Tracking code development and connecting the code version to the results is
critical for reproducibility. To start using Git, open the Terminal and type
<code>git init</code> in your project directory.
"
 }

  return(list(pass = pass, summary = summary, details = details))
}

check_sessioninfo <- function(input, sessioninfo) {
  # Check if the user manually inserted sessionInfo or session_info (from
  # devtools or sessioninfo packages)
  lines <- readLines(input)
  any_sessioninfo <- stringr::str_detect(lines, "session(_i|I)nfo")
  if (any(any_sessioninfo) || sessioninfo != "") {
    pass <- TRUE
    summary <- "<strong>Session information:</strong> recorded"
    details <-
"
Great job! Recording the operating system, R version, and package versions is
critical for reproducibility.
"
  } else {
    pass <- FALSE
    summary <- "<strong>Session information:</strong> unavailable"
    details <-
"
Recording the operating system, R version, and package versions is critical
for reproducibility. To record the session information, add <code>sessioninfo:
\"sessionInfo()\"</code> to _workflowr.yml. Alternatively, you could use
<code>devtools::session_info()</code> or
<code>sessioninfo::session_info()</code>. Lastly, you can manually add a code
chunk to this file to run any one of these commands and then disable to
automatic insertion by changing the workflowr setting to <code>sessioninfo:
\"\"</code>.
"
  }

  return(list(pass = pass, summary = summary, details = details))
}

check_seed <- function(seed) {
  if (is.numeric(seed) && length(seed) == 1) {
    pass <- TRUE
    seed_code <- sprintf("<code>set.seed(%d)</code>", seed)
    summary <- sprintf("<strong>Seed:</strong> %s", seed_code)
    details <- sprintf(
"
The command %s was run prior to running the code in the R Markdown file.
Setting a seed ensures that any results that rely on randomness, e.g.
subsampling or permutations, are reproducible.
"
                       , seed_code)
  } else {
    pass <- FALSE
    summary <- "<strong>Seed:</strong> none"
    details <-
"
No seed was set with <code>set.seed</code> prior to running the code in the R
Markdown file. Setting a seed ensures that any results that rely on
randomness, e.g. subsampling or permutations, are reproducible. To set a seed,
specify an integer value for the option seed in _workflowr.yml or the YAML header
of the R Markdown file.
"
  }

  return(list(pass = pass, summary = summary, details = details))
}

# This function is designed to check the global environment for any defined
# objects that could interfere with an analysis. However, it accepts arbitrary
# environments to facilitate unit testing.
check_environment <- function(envir = .GlobalEnv) {
  ls_envir <- ls(name = envir)
  if (length(ls_envir) == 0) {
    pass <- TRUE
    summary <- "<strong>Environment:</strong> empty"
    details <-
"
Great job! The global environment was empty. Objects defined in the global
environment can affect the analysis in your R Markdown file in unknown ways.
For reproduciblity it's best to always run the code in an empty environment.
"
  } else {
    pass <- FALSE
    summary <- "<strong>Environment:</strong> objects present"
    details <-
"
<p>The global environment had objects present when the code in the R Markdown
file was run. These objects can affect the analysis in your R Markdown file in
unknown ways. For reproduciblity it's best to always run the code in an empty
environment. Use <code>wflow_publish</code> or <code>wflow_build</code> to
ensure that the code is always run in an empty environment.</p>
"
    objects_table <- create_objects_table(envir)
    details <- paste(collapse = "\n",
                     details,
                     "<p>The following objects were defined in the global
                     environment when these results were created:</p>",
                     objects_table)
  }

  return(list(pass = pass, summary = summary, details = details))
}

create_objects_table <- function(env) {
  objects <- ls(name = env)
  classes <- vapply(objects, function(x) paste(class(env[[x]]), collapse = ";"),
                    character(1))
  sizes <- vapply(objects,
                  function(x) format(utils::object.size(env[[x]]), units = "auto"),
                  character(1))
  df <- data.frame(Name = objects, Class = classes, Size = sizes)
  table <- convert_df_to_html_table(df)
  return(table)
}

convert_df_to_html_table <- function(df) {
  table <- knitr::kable(df, format = "html", row.names = FALSE,
                        table.attr = "class=\"table table-condensed table-hover\"")
  return(as.character(table))
}

format_check <- function(check) {
  if (check$pass) {
    symbol <- "glyphicon-ok text-success"
  } else {
    symbol <- "glyphicon-exclamation-sign text-danger"
  }
  # Create a unique ID for the collapsible panel based on the summary by
  # concatenating all alphanumeric characters.
  panel_id <- stringr::str_extract_all(check$summary, "[:alnum:]")[[1]]
  panel_id <- paste(panel_id, collapse = "")
  text <- glue::glue('
  <div class="panel panel-default">
  <div class="panel-heading">
  <p class="panel-title">
  <a data-toggle="collapse" data-parent="#workflowr-checks" href="#{panel_id}">
    <span class="glyphicon {symbol}" aria-hidden="true"></span>
    {check$summary}
  </a>
  </p>
  </div>
  <div id="{panel_id}" class="panel-collapse collapse">
  <div class="panel-body">
    {check$details}
  </div>
  </div>
  </div>
  '
  )
  return(text)
}

check_rmd <- function(input, r, s) {

  stopifnot("ignored" %in% names(s))

  s_simpler <- lapply(s, unlist)
  s_simpler <- lapply(s_simpler, add_git_path, r = r)

  # Determine current status of R Markdown file
  if (input %in% s_simpler$staged) {
    rmd_status <- "staged"
  } else if (input %in% s_simpler$unstaged) {
    rmd_status <- "unstaged"
  } else if (input %in% s_simpler$untracked) {
    rmd_status <- "untracked"
  } else if (input %in% s_simpler$ignored) {
    rmd_status <- "ignored"
  } else {
    rmd_status <- "up-to-date"
  }

  if (rmd_status == "up-to-date") {
    pass <- TRUE
    summary <- "<strong>R Markdown file:</strong> up-to-date"
    details <-
"
Great! Since the R Markdown file has been committed to the Git repository, you
know the exact version of the code that produced these results.
"
  } else {
    pass <- FALSE
    summary <- "<strong>R Markdown file:</strong> uncommitted changes"
    if (rmd_status %in% c("staged", "unstaged")) {
      details <- sprintf("The R Markdown file has %s changes.", rmd_status)
    } else {
      details <- sprintf("The R Markdown is %s by Git.", rmd_status)
    }
    details <- paste(collapse = " ", details,
"
To know which version of the R Markdown file created these
results, you'll want to first commit it to the Git repo. If
you're still working on the analysis, you can ignore this
warning. When you're finished, you can run
<code>wflow_publish</code> to commit the R Markdown file and
build the HTML.
"
                    )
  }

  return(list(pass = pass, summary = summary, details = details))
}

check_cache <- function(input) {
  # Check for cached chunks
  input_cache <- fs::path_ext_remove(input)
  input_cache <- glue::glue("{input_cache}_cache")
  cached_chunks_files <- list.files(path = file.path(input_cache, "html"),
                                    pattern = "RData$")

  if (length(cached_chunks_files) == 0) {
    pass <- TRUE
    summary <- "<strong>Cache:</strong> none"
    details <-
      "
Nice! There were no cached chunks for this analysis, so you can be confident
that you successfully produced the results during this run.
"
  } else {
    pass <- FALSE
    summary <- "<strong>Cache:</strong> detected"

    cached_chunks <- fs::path_file(cached_chunks_files)
    cached_chunks <- stringr::str_replace(cached_chunks, "_[a-z0-9]+.RData$", "")
    cached_chunks <- unique(cached_chunks)
    cached_chunks <- paste0("<li>", cached_chunks, "</li>", collapse = "")

    details <- glue::glue("
The following chunks had caches available: <ul>{cached_chunks}</ul>
To ensure reproducibility of the results, delete the cache directory
<code>{fs::path_rel(input_cache, start = fs::path_dir(input))}</code>
and re-run the analysis. To have workflowr automatically delete the cache
directory prior to building the file, set <code>delete_cache = TRUE</code>
when running <code>wflow_build()</code> or <code>wflow_publish()</code>.
")
  }

  return(list(pass = pass, summary = summary, details = details))
}


add_git_path <- function(x, r) {
  if (!is.null(x)) {
    file.path(git2r::workdir(r), x)
  } else {
   NA_character_
  }
}

detect_code <- function(input) {
  stopifnot(fs::file_exists(input))
  lines <- readLines(input)

  code_chunks <- stringr::str_detect(lines, "^```\\{[a-z].*\\}$")
  # Inline code can span multiple lines, so concatenate first. A new line counts
  # as a character, which is the same as the space inserted by the collapse.
  lines_collapsed <- paste(lines, collapse = " ")
  # Extract all strings that start with "`r " and end with "`" (with no
  # intervening "`").
  code_inline_potential <- stringr::str_extract_all(lines_collapsed, "`r[^`]+`")[[1]]
  # Only keep valid inline code:
  # 1. Must start with at least one whitespace character after the "`r"
  # 2. Must contain at least one non-whitespace character
  #
  # The regex in words is:
  # `r{at least one whitespace character}{at least one non whitespace character}{zero or more characters}`
  code_inline <- stringr::str_detect(code_inline_potential, "`r\\s+\\S+.*`")

  return(any(code_chunks) || any(code_inline))
}

# Create URL to past versions of HTML files.
#
# For workflowr projects hosted at GitHub.com or GitLab.com, the returned URL
# will be to a CDN provided by raw.githack.com. The file is served as HTML for
# convenient viewing of the results. If the project is hosted on a different
# platform (e.g. Bitbucket or a custom GitLab instance), the returned URL will
# be to the specific version of the HTML file in the repository (inconveniently
# rendered as text).
#
# https://raw.githack.com/
#
# Examples:
#
# GitHub: https://github.com/user/repo/blob/commit/path/file.html
# -> https://rawcdn.githack.com/user/repo/commit/path/file.html
#
# GitLab: https://gitlab.com/user/repo/blob/commit/path/file.html
# -> https://glcdn.githack.com/user/repo/raw/commit/path/file.html
#
# GitLab custom: https://git.rcc.uchicago.edu/user/repo/blob/commit/path/file.html
# -> https://git.rcc.uchicago.edu/user/repo/blob/commit/path/file.html
#
# Note: The full result includes the anchor tag:
# <a href=\"https://rawcdn.githack.com/user/repo/commit/path/file.html\" target=\"_blank\">1st 7 characters of commit</a>
create_url_html <- function(url_repo, html, sha) {
  url_github <- "https://github.com/"
  url_gitlab <- "https://gitlab.com/"
  cdn_github <- "https://rawcdn.githack.com"
  cdn_gitlab <- "https://glcdn.githack.com"

  if (stringr::str_detect(url_repo, url_github)) {
    url_html <- sprintf("<a href=\"%s/%s/%s/%s\" target=\"_blank\">%s</a>",
                        cdn_github,
                        stringr::str_replace(url_repo, url_github, ""),
                        sha, html, shorten_sha(sha))
  } else if (stringr::str_detect(url_repo, url_gitlab)) {
    url_html <- sprintf("<a href=\"%s/%s/raw/%s/%s\" target=\"_blank\">%s</a>",
                        cdn_gitlab,
                        stringr::str_replace(url_repo, url_gitlab, ""),
                        sha, html, shorten_sha(sha))
  } else {
    url_html <- sprintf("<a href=\"%s/blob/%s/%s\" target=\"_blank\">%s</a>",
                        url_repo, sha, html, shorten_sha(sha))
  }

  return(url_html)
}

shorten_sha <- function(sha) {
  stringr::str_sub(sha, 1, 7)
}

# Detect absolute file paths in a character vector
#
# Detects Unix and Windows file paths. Paths must be surrounded by quotations
# as they would appear in R code.
#
# Returns a character vector of all potential absolute paths
#
# Returns: "/a/b/c", '/a/b/c', "~/a/b/c", "~\\a\\b\\c", "C:/a/b/c", "C:\\a\\b\\c"
# Ignores: /a/b/c, "~a", "C:a/b/c", "~"
#
# **Warning:** The identified paths may not be returned in the input order
# because the order depends on the order of the regexes that are used to search
# for paths.
#
# Note: Since this checks the entire document, including non-code, I made it
# stringent. For example, it ignores "~" and "C:". These are technically valid
# paths, but it's unlikely that workflowr will be able to provide useful advice
# if these are actually being used as paths. Also, they could be in non-code
# sections. A potential way to improve this check is to first extract the code
# from the document and/or remove comments.
detect_abs_path <- function(string) {
  path_regex <- c("[\",\'](/.+?)[\",\']", # Unix path surrounded by ' or "
                  "[\",\']([a-z,A-Z]:[/,\\\\].+?)[\",\']", # Windows path surrounded by ' or "
                  "[\",\'](~[/,\\\\].+?)[\",\']" # Path with tilde surrounded by ' or "
  )
  paths <- list()
  for (re in path_regex) {
    paths <- c(paths, stringr::str_match_all(string, re))
  }
  paths <- Reduce(rbind, paths)[, 2]

  return(paths)
}

# Check for absolute paths that should be relative paths
#
# Looks for absolute paths between quotation marks (to detect strings in code)
# and between parentheses (to detect links in Markdown syntax). The paths have
# to be within the same project.
check_paths <- function(input, knit_root_dir) {

  # Can't assume a workflowr just because they are using wflow_html().
  proj_dir <- get_proj_dir(knit_root_dir)

  lines <- readLines(input)
  paths <- detect_abs_path(lines)
  # Because fs doesn't remove the ~
  paths_original <- paths
  paths <- absolute(paths)
  names(paths) <- paths_original
  # Remove any duplicates
  paths <- paths[!duplicated(paths)]

  if (length(paths) > 0) {
    internal <- vapply(paths, fs::path_has_parent, logical(1),
                       parent = proj_dir)
    paths <- paths[internal]
  }

  if (length(paths) == 0) {
    pass <- TRUE
    summary <- "<strong>File paths:</strong> relative"
    details <-
      "
Great job! Using relative paths to the files within your workflowr project
makes it easier to run your code on other machines.
"
  } else {
    pass <- FALSE
    summary <- "<strong>File paths:</strong> absolute"
    # List the absolute paths and the suggested relative paths (need to be
    # relative to knit_root_dir)
    paths_df <- data.frame(absolute = names(paths),
                           relative = relative(paths, start = knit_root_dir),
                           stringsAsFactors = FALSE)
    # If the original absolute path uses backslashes on Windows, use backslashes
    # for the suggested relative path. Also display double backslashes as it
    # would appear in R code.
    paths_w_backslash <- stringr::str_detect(paths_df$absolute, "\\\\")
    paths_df$relative[paths_w_backslash] <- stringr::str_replace_all(paths_df$relative[paths_w_backslash],
                                                                     "/", "\\\\\\\\\\\\\\\\")
    paths_df$absolute[paths_w_backslash] <- stringr::str_replace_all(paths_df$absolute[paths_w_backslash],
                                                                     "\\\\\\\\", "\\\\\\\\\\\\\\\\")
    paths_df_html <- convert_df_to_html_table(paths_df)
    details <- glue::glue("
<p>Using absolute paths to the files within your workflowr project makes it
difficult for you and others to run your code on a different machine. Change
the absolute path(s) below to the suggested relative path(s) to make your code
more reproducible.</p>
{paths_df_html}
")
  }

  return(list(pass = pass, summary = summary, details = details))
}

# If uncertain if this is a workflowr project, search for these files in the
# following order to attempt to find root of current project.
#
# *.Rproj
# .git/
# _workflowr.yml
#
# If none of these are present, return the input directory.
get_proj_dir <- function(directory) {

  # RStudio project file, .Rproj
  proj_dir <- try(rprojroot::find_rstudio_root_file(path = directory),
                silent = TRUE)
  if (!inherits(proj_dir, "try-error")) return(proj_dir)

  # .git/
  proj_dir <- try(rprojroot::find_root_file(criterion = rprojroot::is_git_root,
                                            path = directory),
                  silent = TRUE)
  if (!inherits(proj_dir, "try-error")) return(proj_dir)

  # _workflowr.yml file
  proj_dir <- try(rprojroot::find_root(rprojroot::has_file("_workflowr.yml"),
                                       path = directory),
                  silent = TRUE)
  if (!inherits(proj_dir, "try-error")) return(proj_dir)

  return(directory)
}

# Scrub HTML and other generated content (e.g. site_libs). It's ok that these
# have uncommitted changes.
scrub_status <- function(status, repo, output_dir, remove_ignored = FALSE) {
  s <- status_to_df(status)
  full_path <- file.path(git2r::workdir(repo), s$file)
  generated <- vapply(full_path, fs::path_has_parent, logical(1),
                      parent = absolute(output_dir))
  s <- s[!generated, ]
  s <- df_to_status(s)

  # # HTML
  # s <- s[!stringr::str_detect(s$file, "html$"), ]
  # # png
  # s <- s[!stringr::str_detect(s$file, "png$"), ]
  # # site_libs
  # s <- s[!stringr::str_detect(s$file, "site_libs"), ]
  # s <- df_to_status(s)

  if (remove_ignored) s$ignored <- NULL

  return(s)
}
