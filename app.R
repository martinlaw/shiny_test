library(shiny)
library(DT)
library(tidyverse)
library(hms)

# constants that do not change:
a <- 0.1
b <- 0.9
new_alpha <- log(0.5)/10



ui <- basicPage(
  
  # Application title
  titlePanel("PRODOSE2"),
  mainPanel( actionButton("reset", "Reset"),
             numericInput("actual_weight", "Weight", 60),
             numericInput("height", "Height", 170),
             radioButtons("sex", "Sex", c("Female", "Male"), "Male"),
             tags$hr(),
             DTOutput("my_datatable"),
             # verbatimTextOutput("algoweight")
             verbatimTextOutput("weight_ideal"),
             verbatimTextOutput("algo_weight"),
             verbatimTextOutput("UFH_dose_init"),
             verbatimTextOutput("time_of_intervention_init"),
             verbatimTextOutput("half_life")
  )
)

server <- function(input, output) {
  
  # calculate scalars (reactive):
  weight_ideal <- reactive({ifelse(test=input$sex=="Male", yes=(0.9*input$height)-88, no=(0.9*input$height)-92)})
  output$weight_ideal <- renderPrint(weight_ideal())
  
  output$actual_weight <- renderPrint(input$actual_weight)
  
  algo_weight <- reactive({min(input$actual_weight, weight_ideal())})
  output$algo_weight <- renderPrint(algo_weight())

  
  # Assign scalar inputs to their object names etc.
  
  
  #initialize a dataframe
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
  
  # Assign initial values in table to their own objects:
  UFH_dose_init <- reactive({v$data[1,1]})
  time_of_intervention_init <- reactive({v$data[1,2]})
  half_life <- reactive({26+0.323*UFH_dose_init()/algo_weight()})
  
  output$UFH_dose_init <- renderPrint(UFH_dose_init())
  output$time_of_intervention_init <- renderPrint(time_of_intervention_init())
  output$half_life <- renderPrint(half_life())

  
}

# Run the application 
shinyApp(ui = ui, server = server)