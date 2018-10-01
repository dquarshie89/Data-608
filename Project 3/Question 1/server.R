library(shiny)
library(dplyr)
library(googleVis)

data <- 'https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv'
cdc_data <- read.csv(data, na.strings = "NA")

shinyServer(function(input, output) {
  q1 <- reactive(cdc_data %>%
    filter(Year == 2010, ICD.Chapter==input$c) %>%
    select(State, Crude.Rate) %>%
    arrange(desc(Crude.Rate)))
    
  
  output$gvisplot <- renderGvis({
    gvisBarChart(q1(), options=list(width=400, height=450))
                                
  })
  
})



