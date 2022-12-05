library(shiny)
library(DT)
library(tidyverse)
library(hms)

# constants that do not change:
weight_ideal <- data.frame(Male=65, Female=61)
a <- 0.1
b <- 0.9
new_alpha <- log(0.5)/10



ui <- basicPage(
  
  # Application title
  titlePanel("PRODOSE2"),
  mainPanel( actionButton("reset", "Reset"),
             textInput("actual_weight", "Weight"),
             textInput("height", "Height"),
             radioButtons("sex", "Sex", c("Female", "Male")),
             tags$hr(),
             DTOutput("my_datatable")
    )
)

server <- function(input, output) {
  
  # Assign scalar inputs to their object names etc.
  
  
  #initialize a blank dataframe
  v <- reactiveValues(data = { 
    data.frame(UFH_dose = c (28000, 10000, 11000, 0, 0, 0),
               time_of_intervention = rep(hms(hours = 12), 6),
               protamine_dose = rep(NA, 6),
               stringsAsFactors = FALSE)
  })
  
  #output the datatable based on the dataframe (and make it editable)
  output$my_datatable <- renderDT({
    DT::datatable(v$data, editable = TRUE)
  })
  
  #when there is any edit to a cell, write that edit to the initial dataframe
  #check to make sure it's positive, if not convert
  observeEvent(input$my_datatable_cell_edit, {
    #get values
    info = input$my_datatable_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)

    isolate(
        if (j %in% match("protamine_dose", names(v$data))) {
          stop("You are not supposed to change this column.") # check to stop the user from editing only few columns
        }
    )
    
    #write values to reactive
    v$data[i,j] <- k
  })
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)