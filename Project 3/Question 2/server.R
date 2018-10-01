library(shiny)
library(dplyr)
library(googleVis)

data <- 'https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv'
cdc_data <- read.csv(data, na.strings = "NA")

shinyServer(function(input, output) {
  rate <- reactive(cdc_data %>% 
                         filter(ICD.Chapter == input$c) %>%
                         group_by(Year) %>%
                         summarize(Rate = sum(Population * Crude.Rate) / sum(Population)))
  
  change <- reactive(rate() %>% 
                           mutate(Change = (Rate - Rate[Year == 1999]) / Rate[Year == 1999]) %>% 
                           filter(Year == 2010) %>% 
                           select(Change))
  
  q2 <- reactive(cdc_data %>% 
                   filter(ICD.Chapter == input$c) %>% 
                   group_by(State) %>% 
                   select(State, Year, Crude.Rate) %>%
                   mutate(Change = (Crude.Rate - Crude.Rate[Year == 1999]) / Crude.Rate[Year == 1999]) %>% 
                   filter(Year == 2010) %>% 
                   mutate(Rate = Change - change()$'Change') %>%
                   select(State, Rate) %>% 
                   arrange(Rate))
  
  output$gvisplot <- renderGvis({
    gvisBarChart(q2(), options=list(width=400, height=450))
    
  })
})