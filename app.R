library(shiny)
library(DT)
library(tidyverse)
library(hms)
library(lubridate)

# constants that do not change:
a <- 0.1
b <- 0.9
new_alpha <- log(0.5)/10



ui <- basicPage(
  
  # Application title
  titlePanel("PRODOSE2"),
  mainPanel( actionButton("reset", "Reset"),
             actionButton("calculate_prot", "Calculate!"),
             numericInput("actual_weight", "Weight", 60),
             numericInput("height", "Height", 170),
             radioButtons("sex", "Sex", c("Female", "Male"), "Male"),
             tags$hr(),
             DTOutput("my_datatable"),
             # verbatimTextOutput("algoweight") 
             verbatimTextOutput("time_vec"),
             verbatimTextOutput("class_time"),
             verbatimTextOutput("elapsed"),
             verbatimTextOutput("protamine"),
             verbatimTextOutput("weight_ideal"),
             verbatimTextOutput("algo_weight"),
             verbatimTextOutput("UFH_dose_init"),
             verbatimTextOutput("time_of_intervention_init"),
             verbatimTextOutput("half_life"),

             verbatimTextOutput("prot_dose_init"),
             verbatimTextOutput("beta"),
             verbatimTextOutput("C0_init")
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
    data.frame(UFH_dose = c (28000, 10000, 11000,  rep(NA, 3)),
              # time_of_intervention = c(hms(hours = 9, minutes=16), hms(hours = 9, minutes=55), hms(hours = 11, minutes=55), rep(NA, 3)),
              time_of_intervention=c("09:16", "09:55", "11:55", rep(NA, 3)),
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
    k = info$value

    #write values to reactive
    v$data[i,j] <- k
    
    })
  
  # Assign initial values in table to their own objects:
  UFH_dose_init <- reactive({v$data[1,1]})
  C0_init <- reactive({UFH_dose_init()})
  prot_dose_init <- reactive({UFH_dose_init()/100})

  time_of_intervention_init <- reactive({hm(v$data[1,2])})
  half_life <- reactive({26+0.323*UFH_dose_init()/algo_weight()})
  beta <- reactive({log(0.5)/half_life()})
  
  
  output$UFH_dose_init <- renderPrint(UFH_dose_init())
  output$time_of_intervention_init <- renderPrint(time_of_intervention_init())
  output$half_life <- renderPrint(half_life())
  
  output$C0_init <- renderPrint(C0_init())
  output$prot_dose_init <- renderPrint(prot_dose_init())
  output$beta <- renderPrint(beta())

  #no_of_doses_times <- reactive({sum(!is.na(v$data[,2]))}) # number of rows, ie no. of doses to calculate.

# # Reactive expressions required for calculating protamine dose:
#   time_elapsed <- reactive({as.numeric(v$data[-1, 2] - v$data[-length(v$data), 2])/60})
#   no_rows_minus_1 <- reactive({length(time_elapsed())})

# Reactive expressions required for calculating protamine dose:
  output$time_vec <- reactive({v$data[, 2]}) 
  no_rows <- reactive({sum(!is.na(v$data[,2]))})
  no_rows_minus_1 <- reactive({no_rows() - 1})
  time_elapsed <- reactive({as.numeric(hm(v$data[2:no_rows(), 2]) - hm(v$data[1:no_rows_minus_1(), 2]))/60})
  output$elapsed <- renderPrint(time_elapsed())
  
  
# # Calculate protamine dose:
prot_dose_vector <- eventReactive(input$calculate_prot,{
  C0 <- c(C0_init(), rep(NA, no_rows_minus_1()))
  prot_dose <- c(prot_dose_init(), rep(NA, 5))
  for(i in 2:no_rows()){
    C0[i] <- v$data[i, 1] + C0[i-1]*a*exp(new_alpha*time_elapsed()[i-1]) + C0[i-1]*b*exp(beta()*time_elapsed()[i-1])
    prot_dose[i] <- round(C0[i]/100)
  }
  
  v$data[, 3] <- prot_dose
  prot_dose
})

output$class_time <- renderPrint(class(v$data[,2]))


output$protamine <- renderPrint(prot_dose_vector())

  ### Reset data
observeEvent(input$reset, {
    updateNumericInput(inputId = "actual_weight", value = 60)
    updateNumericInput(inputId = "height", value = 170)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)