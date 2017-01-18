#' Create a results page with links to analysis files
#'
#' \code{create_links_page} automates the curation of all the analyses for a
#' project.
#'
#' By default, this creates the file "results.Rmd" in the analysis/
#' subdirectory. If you decide to change this name, be sure to update the
#' navigation bar settings in _site.yml.
#'
#' There are currently 4 methods available for sorting the files. "filename"
#' (the default) sorts by filename, "title" sorts by title (if the R Markdown
#' file does not contain a title in the YAML, the filename is used), "date"
#' sorts in chronological order, and "date reverse" sorts in reverse
#' chronological order.
#'
#' @param output The basename of the results file (default: "results.Rmd")
#' @param sort_method The method for sorting the files. Options include
#'   "filename" (default), "title", "date", and "date reverse" (see Details for
#'   explanation).
#' @param path By default the function assumes the current working directory is
#'   within the project. If this is not true, you'll need to provide the path to
#'   the project directory.
#' @param exclude_rmd character vector of R Markdown files (basename only) to
#'   exclude from the results page
#' @param datatable Display the list of results using DT::datatable (not yet
#'   implemented)
#'
#' @return Invisibly returns a data.frame containing information on the R
#'   Markdown files.
#'
#' @examples
#' \dontrun{
#' create_links_page()
#' }
#' @import rmarkdown
#' @export
create_links_page <- function(output = "results.Rmd",
                           sort_method = "filename",
                           path = ".",
                           exclude_rmd = NULL,
                           datatable = FALSE) {
  analysis_dir <- rprojroot::find_rstudio_root_file("analysis", path = path)
  # Gather Rmd files, only need basename
  rmd_files <- list.files(path = analysis_dir,
                          pattern = utils::glob2rx("*Rmd"))
  # Files to exclude
  excluded <- c("index.Rmd", "about.Rmd", "license.Rmd", "results.Rmd")
  if (!is.null(exclude_rmd)) {
    excluded <- c(excluded, exclude_rmd)
  }
  rmd_files <- rmd_files[!(rmd_files %in% excluded)]

  # Extract YAML info
  titles <- character(length = length(rmd_files))
  dates <- character(length = length(rmd_files))
  authors <- character(length = length(rmd_files))
  for (i in seq_along(rmd_files)) {
    rmd_yaml <- rmarkdown::yaml_front_matter(file.path(analysis_dir,
                                                       rmd_files[i]))
    if (is.null(rmd_yaml$title)) {
      titles[i] <- stringr::str_sub(rmd_files[i], end = -5)
    } else {
      titles[i] <- rmd_yaml$title
    }
    if (is.null(rmd_yaml$date)) {
      dates[i] <- NA
    } else {
      dates[i] <- attempt_date_conversion(rmd_yaml$date, rmd_files[i])
    }
    if (is.null(rmd_yaml$author)) {
      authors[i] <- NA
    } else {
      authors[i] <- rmd_yaml$author
    }
  }
  file_table <- data.frame(rmd = rmd_files,
                           title = titles,
                           date = dates,
                           author = authors,
                           stringsAsFactors = FALSE)

  # Change file extension to html
  file_table$html <- stringr::str_replace(file_table$rmd, "Rmd$", "html")

  if (sort_method == "title") {
    file_table <- file_table[order(file_table$title,
                                   file_table$rmd), ]
  } else if (sort_method == "date") {
    file_table <- file_table[order(file_table$date,
                                   file_table$rmd), ]
  } else if (sort_method == "date reverse") {
    file_table <- file_table[order(file_table$date,
                                   file_table$rmd, decreasing = TRUE), ]
  }

  # Set YAML header
  header <- list()
  header$title <- "Results"
  header$output <- "html_document"
  header <- c("---\n", yaml::as.yaml(header), "---\n\n\n")
  cat(header, file = file.path(analysis_dir, output), sep = "")

  # List results files
  for (i in 1:nrow(file_table)) {
    cat(sprintf("* [%s](%s)\n", file_table$title[i], file_table$html[i]),
        file = file.path(analysis_dir, output), append = TRUE)
  }
  # Add newline at end of file
  # cat("\n", file = file.path(analysis_dir, output), append = TRUE)

  return(invisible(file_table))
}

attempt_date_conversion <- function(x, rmd_fname) {
  # http://mazamascience.com/WorkingWithData/?p=912
  tryCatch({
    tmp_date <- x
    rmd_fname <- rmd_fname
    as.Date(x)},
    error = function(err) {
      warning(sprintf("Unable to convert the date %s found in %s",
                    tmp_date, rmd_fname))
      return(NA)
    }
  )
}
