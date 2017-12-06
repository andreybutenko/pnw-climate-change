library(shiny)
library(dplyr)
library(ggplot2)
library(ggmap)
library(gridExtra)

source('./scripts/visitation_data.R')
source('./scripts/hydro.R')
source('./scripts/hydro-playground.R')
source('./scripts/fss.R')

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
  
  
  output$stream.temp.chart <- renderPlot({
    grid.arrange(historic.stream.temp.chart, future.stream.temp.chart, ncol = 2)
  }, height = 600)
  
  output$stream.temp.diff.chart <- renderPlot({
    diff.stream.temp.chart
  })
  
  output$wa.stream.temp.chart <- renderPlot({
    wa.stream.temp.chart
  })
  
  output$suitability.chart <- renderPlot({
    suitability.data.chart
  })
  

output$visitationPlot <- renderPlotly({  
season <- input$season 
PlotlyGraph(season)
    
  })
  
  
  ############################Spotlight olympic national park #######################
  output$spt.one.plot <- renderPlot({
    visitorFilterGraph(input$spt.one.toggle == 2, input$spt.one.data)
  })
  output$spt.two.ui <- renderUI({
    if (input$spt.one.togglegroup == 2) {
      selectInput("spt.two.data", label = h5("Select Month"), 
                  choices = list("January" = "01", "February" = "02", "March" = "03", "April" = "04", "May" = "05", "June" = "06", "July" = "07", "August" = "08", "September" = "09", "October" = "10", "November" = "11", "December" = "12"), 
                  selected = 1)
    }
  })
  output$spt.two.plot <- renderPlot({
    temperatureGraph(input$spt.one.togglehilow == 1, input$spt.one.togglegroup, input$spt.two.data)
  })
  output$spt.three.plot <- renderPlot({
    airQuality(input$spt.three.data)
  })
  output$spt.one.ui <- renderUI({
    if (input$spt.one.toggle == 2) {
      selectInput("spt.one.data", label = h5("Select"), 
                  choices = list("Any" = "ANY", "1979" = "1979","1980" = "1980","1981" = "1981","1982" = "1982","1983" = "1983","1984" = "1984","1985" = "1985","1986" = "1986","1987" = "1987","1988" = "1988","1989" = "1989","1990" = "1990","1991" = "1991","1992" = "1992","1993" = "1993","1994" = "1994","1995" = "1995","1996" = "1996","1997" = "1997","1998" = "1998","1999" = "1999","2000" = "2000","2001" = "2001","2002" = "2002","2003" = "2003","2004" = "2004","2005" = "2005","2006" = "2006","2007" = "2007","2008" = "2008","2009" = "2009","2010" = "2010","2011" = "2011","2012" = "2012","2013" = "2013","2014" = "2014","2015" = "2015","2016" = "2016"), 
                  selected = 1)
    } else {
      selectInput("spt.one.data", label = h5("Select"), 
                  choices = list("Any" = "ANY", "January" = "Jan", "Febuary" = "Feb", "March" = "Mar", "April" = "Apr", "May" = "May", "June" = "Jun", "July" = "Jul", "August" = "Aug", "September" = "Sep", "October" = "Oct", "November" = "Nov", "December" = "Dec"), 
                  selected = 1)
    }
  })
  ##################################################
})
