library(shiny)
library(dplyr)

source('./scripts/visitation_data.R')
source('./scripts/hydro.R')
source('./scripts/hydro-playground.R')

shinyServer(function(input, output) {
  output$seasonal.runoff <- renderPlot({
    grid.arrange(winter.runoff.historic.chart, summer.runoff.historic.chart, ncol = 2)
  })

  output$snowpack.vs.runoff <- renderPlot({
    snowpack.vs.runoff
  })

  output$snowpack.changes <- renderPlot({
    snowpack.changes
  })

  output$summer.runoff.projections <- renderPlot({
    summer.runoff.diff.historic.chart
  })

  output$hydro.playground.chart <- renderPlot({
    if(input$playground.type == 'monthly') {
      HydroMonthlyPlot(input$playground.measure, input$playground.year)
    } else if(input$playground.type == 'seasonal') {
      HydroSeasonalChart(input$playground.measure, input$playground.year)
    } else if(input$playground.type == 'geo') {
      HydroGeoChart(input$playground.measure, input$playground.year, input$playground.season, input$playground.scenario, input$playground.oregon)
    }
  }, height = 800)

  output$hydro.widgets <- renderUI({
    if(input$playground.type == 'monthly' | input$playground.type == 'seasonal') {
      tagList(
        selectInput('playground.measure', 'Measure', c(
          'Baseflow: Monthly Totals' = 'baseflow',
          'Precipitation: Monthly Totals' = 'precip',
          'Runoff: Monthly Totals' = 'runoff',
          'Snowpack' = 'swe',
          'Avg Temperature' = 'tavg'
        ), selected = 'swe'),
        selectInput('playground.year', 'Year', c(
          '2070',
          '2030'
        ), selected = '2070')
      )
    } else {
      tagList(
        selectInput('playground.measure', 'Measure', c(
          'Baseflow: Monthly Totals' = 'baseflow',
          'Precipitation: Monthly Totals' = 'precip',
          'Runoff: Monthly Totals' = 'runoff',
          'Snowpack' = 'swe',
          'Avg Temperature' = 'tavg'
        ), selected = 'swe'),
        selectInput('playground.year', 'Year', c(
          '2070',
          '2030'
        ), selected = '2070'),
        selectInput('playground.season', 'Season', c(
          'Winter' = 'winter',
          'Spring' = 'spring',
          'Summer' = 'summer',
          'Fall' = 'fall'
        ), selected = 'fall'),
        selectInput('playground.scenario', 'Scenario', c(
          'Higher-Emissions' = 'A1B',
          'Lower-Emissions' = 'B1'
        ), selected = 'A1B'),
        checkboxInput('playground.oregon', 'Include Oregon', value = F)
      )
    }
  })
  
output$visitationPlot <- renderPlotly({
  
season <- input$season 
PlotlyGraph(season)
    
  })
})
