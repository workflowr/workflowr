#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(workflowr)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Workflowr Publish"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(position = "right",
                sidebarPanel(
                  h3("Options"),
                  checkboxInput("all", "all", value = FALSE),
                  checkboxInput("force", "force", value = FALSE),
                  checkboxInput("update", "update", value = FALSE),
                  checkboxInput("republish", "republish", value = FALSE),
                  checkboxInput("view", "view", value = FALSE),
                  checkboxInput("delete_cache", "delete_cache", value = FALSE),
                  checkboxInput("verbose", "verbose", value = FALSE),
                  checkboxInput("dry_run", "dry_run", value = FALSE)




                ),#sidebarpanel
                # Show a plot of the generated distribution
                mainPanel(
                  textInput("file", h3("File Path")),

                  textInput("project", h3("Working directory",
                                          h1(""),helpText("By default the function assumes
                                                          the current working directory is
                                                          within the project. If this is not true,
                                                          you'll need to provide the path to the
                                                          project directory.")),
                            value = ".",
                            width = NULL,
                            placeholder = NULL),

                  numericInput("seed",
                               h3("Seed"),
                               value = 12345),


                  textAreaInput("message", "commit message", placeholder = "Enter your commit message"),
                  verbatimTextOutput("value"),



                  actionButton("go", "Go"),
                  textOutput("selected_var")


                  )
                  )
                )

# Define server logic required to draw a histogram
server <- function(input, output,session) {

  output$selected_var <-  eventReactive(input$go, {
    wflow_publish(
      files = input$files,
      message = input$message,
      all = input$all,
      force = input$force,
      update = input$update,
      republish = input$republish,
      view = interactive(),
      delete_cache = input$delete_cache,
      seed = input$seed,
      verbose = input$verbose,
      dry_run = input$dry_run,
      project = input$project


    )
    paste("Your file has been published! :)")

  })


}

# Run the application
shinyApp(ui = ui, server = server)

