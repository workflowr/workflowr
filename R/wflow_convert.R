#' Convert R Markdown files to workflowr template.
#'
#' \code{wflow_convert} converts existing R Markdown files to use the workflowr
#' template. This is especially useful when migrating an existing project with
#' many R Markdown files to use workflowr.
#'
#' \code{wflow_convert} attempts to identify the current state of each R
#' Markdown file and convert it to the latest version of the workflowr template.
#' Specifically, it can perform any of the following conversions:
#'
#' \itemize{
#'
#' \item Convert a standard R Markdown file to use the workflowr template
#'
#' \item Convert an outdated workflowr Rmd file to use the latest workflowr
#' template
#'
#' \item Convert a workflowr Rmd file to the standalone version of the template
#' (if \code{standalone = TRUE})
#'
#' \item Convert a standalone workflowr Rmd file to the non-standalone version
#' of the template (if \code{standalone = FALSE})
#'
#' \item Convert an \href{https://github.com/jhsiao999/ashlar}{ashlar} R
#' Markdown file to use the workflwor template
#'
#' }
#'
#' If a given R Markdown file is already using the latest version of the
#' template, it is not changed.
#'
#' If a file is lacking a YAML header, a generic YAML header is added.
#'
#' @section Warning: \code{wflow_convert} overwrites the original files.
#'
#' @param files character. The R Markdown file(s) to be converted. Must have
#'   file extension Rmd or rmd. Supports file
#'   \href{https://en.wikipedia.org/wiki/Glob_(programming)}{globbing}.
#' @inheritParams wflow_open
#' @param dry_run logical (default: FALSE). Preview the changes to the files.
#'   Show the output of \code{diff} between the original and the proposed
#'   replacement.
#' @param verbose logical (default: TRUE). Report progress.
#'
#' @return Invisibly returns a list which contains the results of running
#'   \code{diff} between the original and converted versions of each file. The
#'   names of the list elements are the filenames. The diffs are character
#'   vectors. Only files which were successfully converted are included.
#'
#' @seealso \code{\link{wflow_open}}
#'
#' @examples
#' \dontrun{
#'
#' # Convert an R Markdown file to use workflowr template
#' wflow_convert("existing.Rmd")
#' # Convert an R Markdown file to use workflowr template,
#' # but embed the shared chunks to create a standalone file
#' wflow_convert("existing.Rmd", standalone = TRUE)
#' # Preview the changes
#' wflow_convert("existing.Rmd", dry_run = TRUE)
#' # Convert multiple an R Markdown files
#' wflow_convert(files = list.files(path = "analysis",
#'                                  pattern = "Rmd$",
#'                                  full.names = TRUE))
#' }
#'
#' @export
wflow_convert <- function(files,
                          standalone = FALSE,
                          dry_run = FALSE,
                          verbose = TRUE) {
  if (!is.character(files))
    stop("files must be a character vector")
  if (!is.logical(standalone) | length(standalone) != 1)
    stop("standalone must be a one element logical vector")
  if (!is.logical(dry_run) | length(dry_run) != 1)
    stop("dry_run must be a one element logical vector")
  if (!is.logical(verbose) | length(verbose) != 1)
    stop("verbose must be a one element logical vector")

  files <- glob(files)
  files <- relative(files)

  # Check file extensions
  extensions <- tools::file_ext(files)
  non_standard <- !(extensions %in% c("Rmd", "rmd"))
  if (any(non_standard))
    stop("R Markdown files must have the extension Rmd or rmd.")

  # Track successfully converted files. Store their diffs.
  success <- list()

  for (f in files) {
    if (!file.exists(f)) {
      warning("\nSkipping missing file: ", f)
      next
    }
    if (verbose) message("\nProcessing ", f)
    lines_original <- readLines(f)
    lines_converted <- wflow_convert_decide(lines_original, standalone, verbose)
    f_tmp <- tempfile(paste(basename(f), sep = "-"), fileext = ".Rmd")
    cat(lines_converted, file = f_tmp, sep = "\n")
    on.exit(unlink(f_tmp))
    f_tmp <- absolute(f_tmp)
    diffs <- diff_file(from = f, to = f_tmp)
    success[[f]] <- diffs
    if (dry_run) {
      if (verbose) {
        message("\nPotential changes to ", f)
        message(paste(diffs, collapse = "\n"))
      }
    } else {
      cat(lines_converted, file = f, sep = "\n")
      if (verbose) message("Successfully converted ", f)
    }
  }

  return(invisible(success))
}

wflow_convert_decide <- function(lines, standalone, verbose) {

  # Does the file contain a yaml header?
  has_yaml <- lines[1] == "---"
  # Is the file using a previous version of the workflowr template?
  signature_previous <- "workflowr::extract_commit"
  previous <- sum(grepl(signature_previous, lines)) == 1
  # Is the file based on the ashlar template?
  # https://github.com/jhsiao999/ashlar
  signature_ashlar <- "git log -1 --format="
  ashlar <- sum(grepl(signature_ashlar, lines)) == 1
  # Is the file using a current version of the workflowr template?
  signature_current <- "r last-updated, echo=FALSE,"
  current <- sum(grepl(signature_current, lines)) == 1
  # Is the file using a current version of the standalone workflowr template?
  signature_current_stand <- "Unavailable. Initialize Git repository to enable."
  current_stand <- sum(grepl(signature_current_stand, lines)) == 1

  if (!has_yaml)
    lines <- wflow_convert_yaml(lines)

  if (current & current_stand & standalone) {
    # If file is using current standalone workflowr template and `standalone =
    # TRUE`, nothing needs to be done
    if (verbose)
      message("This file is already using the standalone workflowr format")
    return(lines)
  } else if (current & current_stand & !standalone) {
    # If file is using current standalone workflowr template and `standalone =
    # FALSE`, then convert to non-standalone
    newlines <- wflow_convert_to_non_standalone(lines)
  } else if (current & !current_stand & standalone) {
    # If file is using current workflowr template and `standalone =
    # TRUE`, then convert to standalone
    newlines <- wflow_convert_to_standalone(lines)
  } else if (current & !current_stand & !standalone) {
    # If file is using current workflowr template and `standalone =
    # FALSE`, nothing needs to be done
    if (verbose)
      message("This file is already using the current workflowr format")
    return(lines)
  } else if (previous) {
    newlines <- wflow_convert_previous(lines, standalone)
  } else if (ashlar) {
    newlines <- wflow_convert_ashlar(lines, standalone)
  } else {
    newlines <- wflow_convert_standard(lines, standalone)
  }
  return(newlines)
}

wflow_convert_yaml <- function(lines) {
  c(workflowr_yaml, lines)
}

wflow_convert_standard <- function(lines, standalone) {
  dashes <- which(lines == "---")
  if (dashes[1] != 1)
    stop("Input file must contain a YAML header", call. = FALSE)
  if (length(dashes) < 2)
    stop("Unable to locate YAML header", call. = FALSE)
  end_yaml_pos <- dashes[2]
  if (standalone) {
    result <- c(lines[1:end_yaml_pos], # yaml header
                workflowr_front_matter_standalone, # workflowr template
                lines[(end_yaml_pos + 1):length(lines)], # remaining original
                workflowr_end_matter_standalone) # sessionInfo
  } else {
    result <- c(lines[1:end_yaml_pos], # yaml header
                workflowr_front_matter, # workflowr template
                lines[(end_yaml_pos + 1):length(lines)], # remaining original
                workflowr_end_matter) # sessionInfo
  }
  return(result)
}

wflow_convert_previous <- function(lines, standalone) {

  # Identify the read-chunk line
  pos_read_chunk <- grep("read-chunk", lines)
  if (length(pos_read_chunk) != 1)
    stop("Unable to process this file with wflow_convert", call. = FALSE)
  # Identify the "Code version" line
  pos_code_vers <- grep("Code version:", lines)
  if (length(pos_code_vers) != 1)
    stop("Unable to process this file with wflow_convert", call. = FALSE)
  # If the line after is a blank line, set the position to that line. This helps
  # maintain nice spacing but avoids accidentally overwriting content.
  if (lines[pos_code_vers + 1] == "")
    pos_code_vers <- pos_code_vers + 1
  # Identify the session information header H2
  pos_info_h2 <- grep("## Session Information", lines)
  # If the line above is a blank line, set the position to that line. This helps
  # maintain nice spacing but avoids accidentally overwriting content.
  if (lines[pos_info_h2 - 1] == "")
    pos_info_h2 <- pos_info_h2 - 1
  # Identify the session-info chunk
  pos_info_chunk <- grep("session-info", lines)
  # Add 1 since so it includes the ending backticks
  pos_info_chunk <- pos_info_chunk + 1

  # Remove the ashlar-specific lines from the beginning and end of the file
  remove_begin <- -(pos_read_chunk:pos_code_vers)
  remove_end <- -(pos_info_h2:pos_info_chunk)
  newlines <- lines[c(remove_begin, remove_end)]

  wflow_convert_standard(newlines, standalone)
}

wflow_convert_ashlar <-  function(lines, standalone) {

  # Identify the "Last Updated" line
  pos_last_updated <- grep("Last updated:", lines)
  if (length(pos_last_updated) != 1)
    stop("Unable to process this file with wflow_convert", call. = FALSE)
  # Identify the "chunk-options" line
  pos_chunk_opts <- grep("chunk-options.R", lines)
  if (length(pos_chunk_opts) != 1)
    stop("Unable to process this file with wflow_convert", call. = FALSE)
  # Identify the closing backticks of the "chunk-options" chunk
  pos_chunk_opts_end <- grep("```", lines[pos_chunk_opts:length(lines)])[1]
  pos_chunk_opts_end <- pos_chunk_opts + pos_chunk_opts_end
  # Identify the session information header H2
  pos_info_h2 <- grep("## Session information", lines)
  # If the line above is a blank line, set the position to that line. This helps
  # maintain nice spacing but avoids accidentally overwriting content.
  if (lines[pos_info_h2 - 1] == "") {
    pos_info_h2 <- pos_info_h2 - 1
  }
  # Identify the sessionInfo() call
  pos_info_call <- grep("sessionInfo()", lines)
  # Identify the closing backticks of the session information chunk
  pos_info_call_end <- grep("```", lines[pos_info_call:length(lines)])[1]
  pos_info_call_end <- pos_info_call + pos_info_call_end

  # Remove the ashlar-specific lines from the beginning and end of the file
  remove_begin <- -(pos_last_updated:pos_chunk_opts_end)
  remove_end <- -(pos_info_h2:pos_info_call_end)
  newlines <- lines[c(remove_begin, remove_end)]

  wflow_convert_standard(newlines, standalone)
}

wflow_convert_to_standalone <- function(lines) {

  # Identify the read-chunk line
  pos_read_chunk <- grep("read-chunk, include=FALSE, cache=FALSE", lines)
  if (length(pos_read_chunk) != 1)
    stop("Unable to process this file with wflow_convert", call. = FALSE)
  # Include the HTML comment above if the user hasn't deleted it
  if (stringr::str_sub(lines[pos_read_chunk - 2], 1, 4)  == "<!--") {
    pos_read_chunk <- pos_read_chunk - 2
  }
  # Identify the code-version line
  pos_code_vers <- grep("r code-version, echo=FALSE, results=", lines)
  if (length(pos_code_vers) != 1)
    stop("Unable to process this file with wflow_convert", call. = FALSE)
  # Add 1 to include the closing backticks of this chunk
  pos_code_vers <- pos_code_vers + 1
  # If the line after is a blank line, set the position to that line. This helps
  # maintain nice spacing but avoids accidentally overwriting content.
  if (lines[pos_code_vers + 1] == "")
    pos_code_vers <- pos_code_vers + 1
  # Identify the session information header H2
  pos_info_h2 <- grep("## Session information", lines)
  # If the line above is a blank line, set the position to that line. This helps
  # maintain nice spacing but avoids accidentally overwriting content.
  if (lines[pos_info_h2 - 1] == "") {
    pos_info_h2 <- pos_info_h2 - 1
  }
  # Identify the session-info chunk
  pos_info <- grep("r session-info", lines)
  if (length(pos_info) != 1)
    stop("Unable to process this file with wflow_convert", call. = FALSE)
  # Add 1 to include the closing backticks
  pos_info <- pos_info + 1

  # Remove the workflowr-specific lines from the beginning and end of the file
  remove_begin <- -(pos_read_chunk:pos_code_vers)
  remove_end <- -(pos_info_h2:pos_info)
  newlines <- lines[c(remove_begin, remove_end)]

  wflow_convert_standard(newlines, standalone = TRUE)
}

wflow_convert_to_non_standalone <- function(lines) {

  # Identify the knitr-opts-chunk line
  pos_knitr_chunk <- grep("r knitr-opts-chunk, include=FALSE", lines)
  if (length(pos_knitr_chunk) != 1)
    stop("Unable to process this file with wflow_convert", call. = FALSE)
  # Identify the code-version chunk
  pos_code_vers <- grep("r code-version, echo=FALSE, results=", lines)
  if (length(pos_code_vers) != 1)
    stop("Unable to process this file with wflow_convert", call. = FALSE)
  # Identify the closing backticks of the code-version chunk
  pos_code_vers_end <- grep("```", lines[(pos_code_vers + 1):length(lines)])[1]
  pos_code_vers_end <- pos_code_vers + pos_code_vers_end
  # If the line after is a blank line, set the position to that line. This helps
  # maintain nice spacing but avoids accidentally overwriting content.
  if (lines[pos_code_vers_end + 1] == "")
    pos_code_vers_end <- pos_code_vers_end + 1
  # Identify the session information header H2
  pos_info_h2 <- grep("## Session information", lines)
  # If the line above is a blank line, set the position to that line. This helps
  # maintain nice spacing but avoids accidentally overwriting content.
  if (lines[pos_info_h2 - 1] == "") {
    pos_info_h2 <- pos_info_h2 - 1
  }
  # Identify the sessionInfo() call
  pos_info_call <- grep("sessionInfo()", lines)
  if (length(pos_info_call) != 1)
    stop("Unable to process this file with wflow_convert", call. = FALSE)
  # Add 1 to include the closing backticks
  pos_info_call <- pos_info_call + 1

  # Remove the workflowr-specific lines from the beginning and end of the file
  remove_begin <- -(pos_knitr_chunk:pos_code_vers_end)
  remove_end <- -(pos_info_h2:pos_info_call)
  newlines <- lines[c(remove_begin, remove_end)]

  wflow_convert_standard(newlines, standalone = FALSE)
}

workflowr_yaml <- c(
  "---",
  "title: \"Untitled\"",
  "author: \"First Last\"",
  "date: YYYY-MM-DD",
  "output: html_document",
  "---",
  "")

workflowr_front_matter <- c(
  "",
  "<!-- The file analysis/chunks.R contains chunks that define default settings",
  "shared across the workflowr files. -->",
  "```{r read-chunk, include=FALSE, cache=FALSE}",
  "knitr::read_chunk(\"chunks.R\")",
  "```",
  "",
  "<!-- Update knitr chunk options -->",
  "```{r knitr-opts-chunk, include=FALSE}",
  "```",
  "",
  "<!-- Insert the date the file was last updated -->",
  "```{r last-updated, echo=FALSE, results='asis'}",
  "```",
  "",
  "<!-- Insert the code version (Git commit SHA1) if Git repository exists and R",
  " package git2r is installed -->",
  "```{r code-version, echo=FALSE, results='asis'}",
  "```")

workflowr_end_matter <- c(
  "",
  "## Session information",
  "",
  "<!-- Insert the session information into the document -->",
  "```{r session-info}",
  "```")

workflowr_front_matter_standalone <- c(
  "",
  "```{r knitr-opts-chunk, include=FALSE}",
  "# Update knitr chunk options",
  "# https://yihui.name/knitr/options/#chunk-options",
  "knitr::opts_chunk$set(",
  "  comment = NA,",
  "  fig.align = \"center\",",
  "  tidy = FALSE,",
  "  fig.path = paste0(\"figure/\", knitr::current_input(), \"/\")",
  ")",
  "```",
  "",
  "```{r last-updated, echo=FALSE, results='asis'}",
  "# Insert the date the file was last updated",
  "cat(sprintf(\"**Last updated:** %s\", Sys.Date()))",
  "```",
  "",
  "```{r code-version, echo=FALSE, results='asis'}",
  "# Insert the code version (Git commit SHA1) if Git repository exists and R",
  "# package git2r is installed", "if(requireNamespace(\"git2r\", quietly = TRUE)) {",
  "  if(git2r::in_repository()) {",
  "    code_version <- substr(git2r::commits()[[1]]@sha, 1, 7)",
  "  } else {",
  "    code_version <- \"Unavailable. Initialize Git repository to enable.\"",
  "  }",
  "} else {",
  "  code_version <- \"Unavailable. Install git2r package to enable.\"",
  "}",
  "cat(sprintf(\"**Code version:** %s\", code_version))",
  "rm(code_version)",
  "```")

workflowr_end_matter_standalone <- c(
  "",
  "## Session information",
  "",
  "<!-- Insert the session information into the document -->",
  "```{r session-info}",
  "sessionInfo()",
  "```")
