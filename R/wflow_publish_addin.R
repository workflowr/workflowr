
# img(src = "https://raw.githubusercontent.com/workflowr/workflowr-assets/master/img/hex-workflowr.png", height = "50px")

# library(shiny)

wflow_publish_addin <- function() {

  # check for shiny and miniUI
  # Check working directory

  s <- wflow_status()
  wd <- getwd()

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Publish workflowr website",
                           right = miniUI::miniTitleBarButton("done", "Publish", primary = TRUE)),
    miniUI::miniContentPanel(
      shiny::sidebarLayout(position = "right",
        shiny::sidebarPanel(
          shiny::h3("Options"),
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

      cmd_parts <- c(" workflowr::wflow_publish(")
      if (!is.null(input$files)) {
        files_string <- paste(utils::capture.output(dput(input$files)), collapse = "")
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
