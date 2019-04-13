# RStudio Addin (Shiny Gadget) for wflow_publish().
wflow_publish_addin <- function() {

  if (!requireNamespace("miniUI", quietly = TRUE))
    stop("The miniUI package is required to use this RStudio addin.\n",
         "Please install with install.packages(\"miniUI\")",
         call. = FALSE)

  s <- wflow_status()
  site_yml_path <- relative(file.path(s$analysis, "_site.yml"))
  wflow_yml_path <- relative(file.path(s$root, "_workflowr.yml"))

  flower_url <- "https://raw.githubusercontent.com/workflowr/workflowr-assets/master/img/flower-purple.png"
  logo_url <- "https://raw.githubusercontent.com/workflowr/workflowr-assets/master/img/logo-workflowr-inverse.png"

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Publish workflowr website",
                           right = miniUI::miniTitleBarButton("done", "Publish",
                                                              primary = TRUE)),
    miniUI::miniContentPanel(
      shiny::sidebarLayout(position = "right",
        shiny::sidebarPanel(
          shiny::h3(shiny::img(src = flower_url, height = "50px"), "Options"),
          shiny::checkboxInput("all", "all", value = FALSE),
          shiny::checkboxInput("force", "force", value = FALSE),
          shiny::checkboxInput("update", "update", value = FALSE),
          shiny::checkboxInput("republish", "republish", value = FALSE),
          shiny::checkboxInput("delete_cache", "delete_cache", value = FALSE),
          shiny::checkboxInput("dry_run", "dry_run", value = FALSE)
        ),
        shiny::mainPanel(
          shiny::selectInput(inputId = "files",
                             label = "Select files to publish:",
                             choices = rownames(s$status),
                             selected = NULL,
                             multiple = TRUE),
          shiny::checkboxInput("site_yml",
                               glue::glue("Include {site_yml_path}"),
                               value = s$site_yml),
          shiny::checkboxInput("wflow_yml",
                               glue::glue("Include {wflow_yml_path}"),
                               value = !is.null(s$wflow_yml) && s$wflow_yml),
          shiny::textAreaInput(inputId = "message",
                        label = "Describe the changes you made",
                        placeholder = "Enter your commit message")
        )
      ),
      shiny::p("Click the Publish button to execute the following command:"),
      shiny::verbatimTextOutput("cmd_to_run")
    )
  )
  server <- function(input, output, session) {

    cmd <- shiny::reactive({

      files <- c(input$files)
      if (input$site_yml) files <- c(site_yml_path, files)
      if (input$wflow_yml) files <- c(wflow_yml_path, files)

      cmd_parts <- c(" workflowr::wflow_publish(")
      if (!is.null(files)) {
        files_string <- paste(utils::capture.output(dput(files)), collapse = "")
        cmd_parts <- c(cmd_parts, glue::glue("\n   files = {files_string},", .trim = FALSE))
      }
      if (input$message != "") cmd_parts <- c(cmd_parts, glue::glue("\n   message = \"{input$message}\",", .trim = FALSE))
      if (input$all) cmd_parts <- c(cmd_parts, "\n   all = TRUE,")
      if (input$force) cmd_parts <- c(cmd_parts, "\n   force = TRUE,")
      if (input$update) cmd_parts <- c(cmd_parts, "\n   update = TRUE,")
      if (input$republish) cmd_parts <- c(cmd_parts, "\n   republish = TRUE,")
      if (input$delete_cache) cmd_parts <- c(cmd_parts, "\n   delete_cache = TRUE,")
      if (input$dry_run) cmd_parts <- c(cmd_parts, "\n   dry_run = TRUE,")
      cmd_parts[length(cmd_parts)] <- stringr::str_replace(cmd_parts[length(cmd_parts)],
                                                           ",$", "")
      cmd_parts <- c(cmd_parts, ")")

      return(cmd_parts)
    })

    output$cmd_to_run <- shiny::renderText({ cmd() })

    shiny::observeEvent(input$done, {
      shiny::stopApp()
      rstudioapi::sendToConsole(cmd())
    })
  }

  viewer <- shiny::dialogViewer("wflow_publish()", width = 1000, height = 800)
  shiny::runGadget(ui, server, viewer = viewer)
}
