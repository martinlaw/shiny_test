library(shiny)
library(DT)
library(tidyverse)
library(hms)



ui <- fluidPage(
  
  # Application title
  titlePanel("Editable Dataframe and Plot"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      DTOutput("my_datatable"),
      actionButton("go",label = "Plot Data")
    ),
    
    # Show plot
    mainPanel(
      plotOutput("my_plot")
    )
  )
)

server <- function(input, output) {
  
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
        }else{
          
        }
    )
    
    #write values to reactive
    v$data[i,j] <- k
  })
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)