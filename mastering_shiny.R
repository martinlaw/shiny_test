# install.packages(c(
#   "gapminder", "ggforce", "gh", "globals", "openintro", "profvis", 
#   "RSQLite", "shiny", "shinycssloaders", "shinyFeedback", 
#   "shinythemes", "testthat", "thematic", "tidyverse", "vroom", 
#   "waiter", "xml2", "zeallot" 
# ))
# packageVersion("shiny")
library(shiny)


ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table")
)

server <- function(input, output, session) {
  output$summary <- renderPrint({
    dataset <- get(input$dataset, "package:datasets")
    summary(dataset)
  })
  
  output$table <- renderTable({
    dataset <- get(input$dataset, "package:datasets")
    dataset
  })
}

shinyApp(ui, server)

# The UI is what you see on the final page: an input selector and two outputs,
# one for text (verbatimTextOutput) called "summary" and one for a table (tableOutput)
# called "table".

# In server, the two outputs named in UI ("summary" and "table") have corresponding
# assignments, inside the object "output". They are each assigned to be some kind of
# function with the prefix render. Each render fn. produces a different kind of output.

# input$dataset is referred to in the output fns. This means that when input$dataset is 
# changed (by choosing a different dataset in the app), Shiny knows to update the outputs.

# We use the line

#     dataset <- get(input$dataset, "package:datasets")

# twice, which is wastful and unnecessary. We can improve the code by creating a 
# "reactive expression" (like a function, but it updates when the input updates).
# By assigning a reactive expression, we make the above code much neater:


# server <- function(input, output, session) {
#   # Create a reactive expression
#   dataset <- reactive({
#     get(input$dataset, "package:datasets")
#   })
#   
#   output$summary <- renderPrint({
#     # Use a reactive expression by calling it like a function
#     summary(dataset())
#   })
#   
#   output$table <- renderTable({
#     dataset()
#   })
# }