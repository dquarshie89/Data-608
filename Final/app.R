library(shiny)
library(shinyjs)
library(V8)
library(plotly)
library(tidyverse)
library(shinythemes)
library(viridis)
library(markdown)

load('final.RData')

# set variables for custom plot creation
plot_vars <- list('# of New Businesses Created' = 'TotalNewBusinesses',
                  'Median Home Price' = 'MedianPrice',
                  '% Change in Home Prices' = 'PriceChange')

cities <- c('All',"Manhattan","Brooklyn","Bronx","Queens","Staten Island","Arlington","Washington")

zip <- sort(unique(final$ZipCode))

server <- shinyServer(function(input, output) {
  # create motion chart based on selections
  output$pyMotionChart <- renderPlotly({
    ({if(input$city == 'All') {
      final
    } else {
      final %>% filter(City == input$city)
    }}) %>%
      #Create scatterplot
      plot_ly(x = as.formula(paste0('~', input$x_mot)),
              y = as.formula(paste0('~', input$y_mot)),
              text = ~ZipCode,  alpha = 0.75,
              height = 600) %>%
      group_by(ZipCode) %>%
      # Scale graph
      layout(xaxis = list(type = input$mot_scale_x),
             yaxis = list(type = input$mot_scale_y)) %>%
      #Add Color
      add_markers(color = as.formula(paste0('~', input$color_mot)), 
                  frame = ~Year, colors = rev(viridis(4))) %>%
      # Animate
      animation_opts(frame = 2000, easing = 'cubic-in-out') %>%
      layout(margin = list(l = 100, r = 100, b = 50, t = 50, pad = 1))
    
  })
  
  # create line chart based on selections
  output$pyLineChart <- renderPlotly({
    final %>% 
      # select neighborhood based on selection
      filter(Neighborhood == input$neighborhood) %>%
      # create plotly line graph based on selected y variable
      plot_ly(x = ~Year, y = as.formula(paste0('~', input$y_lin)),
              hoverinfo = "all", 
              hovertext = ~ZipCode,
              color = ~City, colors = rev(viridis(4))) %>%
      group_by(ZipCode) %>% add_lines() %>%
      # set layout parameters
      layout(margin = list(l = 100, r = 100, b = 50, t = 50, pad = 1),
             yaxis = list(type = input$lin_scale), hovermode = 'closest')
  })
  
 
})

ui <- shinyUI(
  navbarPage(theme = shinytheme("superhero"),
             "Changes in Home Prices due to Business Growth",
             tabPanel("Charts", 
                      fluidPage(
                        sidebarPanel(
                          # Create Line Graph Layout
                          conditionalPanel(condition="input.conditionedPanels==2",
                                           selectInput(inputId = 'y_lin', 
                                                       label = 'Select Variable to Plot', 
                                                       choices = plot_vars,
                                                       selected = 'MedianPrice'),

                                           selectInput(inputId = 'neighborhood', 
                                                       label = 'Select Neighborhood to Plot',
                                                       choices = sort(
                                                         unique(final$Neighborhood)
                                                       ),
                                                       selected = 'Northwest Queens'),
                                           selectInput(inputId = 'zip', 
                                                       label = 'Select Zip to Plot',
                                                       choices = sort(unique(final$ZipCode)))
                          ),
                          # Create Date Motion Slider Plot Layout
                          conditionalPanel(condition="input.conditionedPanels==1",
                                           selectInput(inputId = 'x_mot',
                                                       label = 'Select X Axis',
                                                       choices = plot_vars,
                                                       selected = 'TotalNewBusinesses'),
                                           radioButtons(inputId = 'mot_scale_x', 
                                                        label = 'Scale for x axis', 
                                                        choices = list(
                                                          'Linear' = 'linear',
                                                          'Logarithmic' = 'log'),
                                                        selected = 'log',
                                                        inline = TRUE),
                                           selectInput(inputId = 'y_mot',
                                                       label = 'Select Y Axis',
                                                       choices = plot_vars,
                                                       selected = 'MedianPrice'),
                                           radioButtons(inputId = 'mot_scale_y', 
                                                        label = 'Scale for y axis', 
                                                        choices = list(
                                                          'Linear' = 'linear',
                                                          'Logarithmic' = 'log'),
                                                        inline = TRUE),
                                           selectInput(inputId = 'city', 
                                                       label = 'Select City to Plot',
                                                       choices = cities,
                                                       selected = 'All'),
                                           selectInput(inputId = 'color_mot',
                                                       label = 'Select Color Variable',
                                                       choices = c(list(
                                                         'City' = 
                                                           'City',
                                                         'Neighborhood'=
                                                           'Neighborhood',
                                                         '# of New Businesses Created' = 
                                                           'TotalNewBusinesses',
                                                         'Median Home Price' = 
                                                           'MedianPrice',
                                                         '% Change in Home Prices' = 
                                                           'PriceChange')),
                                                       selected = 'City')
                          )
                        ),
                        mainPanel(
                          tabsetPanel(
                            # show motion chart
                            tabPanel("Line Chart", value=2, 
                                     plotlyOutput('pyLineChart')),
                            # show line chart
                            tabPanel("Motion Chart", value=1, 
                                     plotlyOutput('pyMotionChart')),
                            id = "conditionedPanels"
                          )
                        ))
             ),
             # tab containing project information
             tabPanel("About",
                      fluidPage(
                        includeMarkdown("About.Rmd"))))
)

shinyApp(ui, server)