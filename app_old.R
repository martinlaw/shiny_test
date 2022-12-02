#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyTime)
ui <- fluidPage(  # define the User Interface: the HTML page that ppl interact with.
  textInput("weight", "Weight"),
  radioButtons("sex", "Sex", c("F", "M")),
  timeInput("time", "Time 1:", value = Sys.time(), seconds = FALSE),
  timeInput("time2", "Time 2:", value = Sys.time(), seconds = FALSE),
  timeInput("time3", "Time 3:", value = Sys.time(), seconds = FALSE),
  timeInput("time4", "Time 4:", value = Sys.time(), seconds = FALSE),
  tableOutput("table")
)

server <- function(input, output, session) { # specifies the behaviour of the app
  # Create a reactive expression
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  

  
  output$table <- renderTable({
    dataset()
  })
}


shinyApp(ui, server) # Constructs and starts the Shiny app from the UI and server.

