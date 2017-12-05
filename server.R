library(shiny)

source('./scripts/visitation_data.R')

shinyServer(function(input, output) {
  
output$distPlot <- renderPlotly({
  
season <- input$season 
    
    
  })
})
