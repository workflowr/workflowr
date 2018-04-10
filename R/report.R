
create_report <- function(input, output_dir, has_code, opts) {

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

  # workflowr checks -----------------------------------------------------------
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
  }

  # Check version control
  checks$result_vc <- check_vc(input, r, s, opts$github)

  # Formatting checks ----------------------------------------------------------

  checks_formatted <- Map(format_check, checks)
  template_checks <-
"
<strong>workflowr checks:</strong> <small>(Click a bullet for more information)</small>
<ul>
{{{checks}}}
</ul>
"
  data_checks <- list(checks = paste(unlist(checks_formatted), collapse = "\n"))
  report_checks <- whisker::whisker.render(template_checks, data_checks)

  # Version history ------------------------------------------------------------

  if (uses_git) {
    blobs <- git2r::odb_blobs(r)
    versions <- get_versions(input, output_dir, blobs, r, opts$github)
    if (versions == "") {
      report_versions <- versions
    } else {
      template_versions <-
"
<details>
<summary>
<small><strong>Expand here to see past versions:</strong></small>
</summary>
<ul>
{{{versions}}}
</ul>
</details>
"
      report_versions <- whisker::whisker.render(template_versions,
                                                 data = list(versions = versions))
    }
  } else {
    report_versions <- ""
  }

  # Return ---------------------------------------------------------------------

  report <- paste(report_checks, report_versions, collapse = "\n")

  return(report)
}

get_versions <- function(input, output_dir, blobs, r, github) {

  blobs$fname <- file.path(git2r::workdir(r), blobs$path, blobs$name)
  blobs$fname <- absolute(blobs$fname)
  blobs$ext <- tools::file_ext(blobs$fname)

  html <- to_html(input, outdir = output_dir)
  blobs_file <- blobs[blobs$fname %in% c(input, html),
                      c("ext", "commit", "author", "when")]
  # Ignore blobs that don't map to commits (caused by `git commit --amend`)
  git_log <- git2r::commits(r)
  git_log_sha <- vapply(git_log, function(x) x@sha, character(1))
  blobs_file <- blobs_file[blobs_file$commit %in% git_log_sha, ]
  # Exit early if there are no past versions
  if (nrow(blobs_file) == 0) {
    return("")
  }
  colnames(blobs_file) <- c("File", "Version", "Author", "Date")
  blobs_file <- blobs_file[order(blobs_file$Date, decreasing = TRUE), ]
  blobs_file$Date <- as.Date(blobs_file$Date)
  blobs_file$Message <- vapply(blobs_file$Version,
                               get_commit_title,
                               "character(1)",
                               r = r)
  git_html <- stringr::str_replace(html, git2r::workdir(r), "")
  git_rmd <- stringr::str_replace(input, git2r::workdir(r), "")

  if (is.na(github)) {
    blobs_file$Version <- shorten_sha(blobs_file$Version)
  } else {
    # rawgit URL is https://cdn.rawgit.com/user/repo/commit/filepath
    # https://github.com/rgrove/rawgit/blob/master/FAQ.md
    rawgit <- "https://cdn.rawgit.com"
    blobs_file$Version <- ifelse(blobs_file$File == "html",
                                 # HTML preview URL
                                 sprintf("<a href=\"%s/%s/%s/%s\" target=\"_blank\">%s</a>",
                                         rawgit,
                                         stringr::str_replace(github, "https://github.com/", ""),
                                         blobs_file$Version,
                                         git_html, shorten_sha(blobs_file$Version)),
                                 # R Markdown GitHub URL
                                 sprintf("<a href=\"%s/blob/%s/%s\" target=\"_blank\">%s</a>",
                                         github, blobs_file$Version, git_rmd,
                                         shorten_sha(blobs_file$Version)))
  }

  template <-
"
<table style = \"border-collapse:separate; border-spacing:5px;\">
<thead>
<tr>
<th style=\"text-align:left;\"> File </th>
<th style=\"text-align:left;\"> Version </th>
<th style=\"text-align:left;\"> Author </th>
<th style=\"text-align:left;\"> Date </th>
<th style=\"text-align:left;\"> Message </th>
</tr>
</thead>
<tbody>
{{#blobs_file}}
<tr>
<td style=\"text-align:left;\"> {{{File}}} </td>
<td style=\"text-align:left;\"> {{{Version}}} </td>
<td style=\"text-align:left;\"> {{Author}} </td>
<td style=\"text-align:left;\"> {{Date}} </td>
<td style=\"text-align:left;\"> {{Message}} </td>
</tr>
{{/blobs_file}}
</tbody>
</table>
"
  data <- list(blobs_file = unname(whisker::rowSplit(blobs_file)))
  text <- whisker::whisker.render(template, data)

  return(text)
}

# Get versions table for figures. Needs to be refactored to share code with
# get_versions.
get_versions_fig <- function(fig, r, github) {
  fig <- absolute(fig)
  blobs <- git2r::odb_blobs(r)
  blobs$fname <- ifelse(blobs$path == "", blobs$name,
                        file.path(blobs$path, blobs$name))
  blobs$fname_abs <- paste0(git2r::workdir(r), blobs$fname)
  blobs_file <- blobs[blobs$fname_abs == fig, ]
  # Ignore blobs that don't map to commits (caused by `git commit --amend`)
  git_log <- git2r::commits(r)
  git_log_sha <- vapply(git_log, function(x) x@sha, character(1))
  blobs_file <- blobs_file[blobs_file$commit %in% git_log_sha, ]

  # Exit early if there are no past versions
  if (nrow(blobs_file) == 0) {
    return("")
  }

  if (is.na(github)) {
    blobs_file$commit <- shorten_sha(blobs_file$commit)
  } else {
    blobs_file$commit <- sprintf("<a href=\"%s/blob/%s/%s\" target=\"_blank\">%s</a>",
                                 github, blobs_file$commit,
                                 blobs_file$fname,
                                 shorten_sha(blobs_file$commit))
  }

  blobs_file <- blobs_file[, c("commit", "author", "when")]
  colnames(blobs_file) <- c("Version", "Author", "Date")
  blobs_file <- blobs_file[order(blobs_file$Date, decreasing = TRUE), ]
  blobs_file$Date <- as.Date(blobs_file$Date)

  template <-
    "
  <details>
  <summary><em>Expand here to see past versions of {{fig}}:</em></summary>
  <table style = \"border-collapse:separate; border-spacing:5px;\">
  <thead>
  <tr>
  <th style=\"text-align:left;\"> Version </th>
  <th style=\"text-align:left;\"> Author </th>
  <th style=\"text-align:left;\"> Date </th>
  </tr>
  </thead>
  <tbody>
  {{#blobs_file}}
  <tr>
  <td style=\"text-align:left;\"> {{{Version}}} </td>
  <td style=\"text-align:left;\"> {{Author}} </td>
  <td style=\"text-align:left;\"> {{Date}} </td>
  </tr>
  {{/blobs_file}}
  </tbody>
  </table>
  </details>
  "
  data <- list(fig = basename(fig),
               blobs_file = unname(whisker::rowSplit(blobs_file)))
  text <- whisker::whisker.render(template, data)

  return(text)

}


get_commit_title <- function(x, r) {
  full <- git2r::lookup(r, x)@message
  title <- stringr::str_split(full, "\n")[[1]][1]
  return(title)
}

check_vc <- function(input, r, s, github) {
 if (!is.null(r)) {
   pass <- TRUE
   log <- git2r::commits(r)
   if (length(log) > 0) {
     sha <- log[[1]]@sha
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
   s <- status_to_df(s)
   # HTML
   s <- s[!stringr::str_detect(s$file, "html$"), ]
   # png
   s <- s[!stringr::str_detect(s$file, "png$"), ]
   # site_libs
   s <- s[!stringr::str_detect(s$file, "site_libs"), ]
   s <- df_to_status(s)

   status <- utils::capture.output(print(s))
   status <- c("<pre><code>", status, "</code></pre>")
   status <- paste(status, collapse = "\n")
   details <- paste(collpase = "\n",
"
Great! You are using Git for version control. Tracking code development and
connecting the code version to the results is critical for reproducibility.
The version displayed above was the version of the Git repository at the time
these results were generated.
<br><br>
Note that you need to be careful to ensure that all relevant files for the
analysis have been committed to Git prior to generating the results (you can
use <code>wflow_publish</code> or <code>wflow_git_commit</code>). workflowr only
checks the R Markdown file, but you know if there are other scripts or data
files that it depends on. Below is the status of the Git repository when the
results were generated:
"
                , status,
"Note that any generated files, e.g. HTML, png, CSS, etc., are not included in
this status report because it is ok for generated content to have uncommitted
changes.")
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
The global environment had objects present when the code in the R Markdown
file was run. These objects can affect the analysis in your R Markdown file in
unknown ways. For reproduciblity it's best to always run the code in an empty
environment. Use <code>wflow_publish</code> or <code>wflow_build</code> to
ensure that the code is always run in an empty environment.
"
    objects_table <- create_objects_table(envir)
    details <- paste(collapse = "\n",
                     details,
                     "<br><br>",
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
  table <- knitr::kable(df, format = "html", row.names = FALSE)
  # Add table formatting
  table <- stringr::str_replace(table, "<table>",
            "<table style = \"border-collapse:separate; border-spacing:5px;\">")
  return(as.character(table))
}

format_check <- function(check) {
  if (check$pass) {
    symbol <- "<strong style=\"color:blue;\">&#10004;</strong>"
  } else {
    symbol <- "<strong style=\"color:red;\">&#10006;</strong>"
  }
  template <-
    "
  <li>
  <details>
  <summary>
  {{{symbol}}} {{{summary}}}
  </summary>
  {{{details}}}
  </details>
  </li>
  "
  data <- list(symbol = symbol, summary = check$summary,
               details = check$details)
  text <- whisker::whisker.render(template, data)
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

add_git_path <- function(x, r) {
  if (!is.null(x)) {
    paste0(git2r::workdir(r), x)
  } else {
   NA_character_
  }
}

detect_code <- function(input) {
  stopifnot(file.exists(input))
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

shorten_sha <- function(sha) {
  stringr::str_sub(sha, 1, 7)
}
