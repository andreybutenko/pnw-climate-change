library(shiny)


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
    }
    if(input$playground.type == 'seasonal') {
      HydroSeasonalChart(input$playground.measure, input$playground.year)
    }
  })
  
  output$hydro.widgets<- renderUI({
    if(input$playground.type == 'monthly' | input$playground.type == 'seasonal') {
      return(tagList(
        selectInput('playground.measure', 'Measure', c(
          'Baseflow: Monthly Totals' = 'baseflow_monthly_tot',
          'Precipitation: Monthly Totals' = 'precip_monthly_tot',
          'Runoff: Monthly Totals' = 'runoff_monthly_tot',
          'Snowpack' = 'swe_monthly_day1',
          'Avg Temperature' = 'tavg_monthly'
        ), selected = 'swe_monthly_day1'),
        selectInput('playground.year', 'Year', c(
          '2070-2099',
          '2030-2059'
        ), selected = '2070-2099')
      ))
    }
  })
  
  output$distPlot <- renderPlotly({
  
      l <- list(
      font = list(
        family = "sans-serif",
        size = 12,
        color = "#000"),
      bgcolor = "#E2E2E2",
      bordercolor = "#FFFFFF",
      borderwidth = 3,
      orientation = 'h',
      x = 0.5,
      y= -.13)
      
      m <- list(
        l = 50,
        r = 50,
        b = 100,
        t = 100,
        pad = 4)
      
    if(input$season == 'all') {
      plot_ly(all.data, x = ~Year, y = ~all.sum, name = 'Mount Ranier National Park', type = 'scatter', mode = 'lines+markers', hoverinfo = 'text',
              text = ~paste(Year,', ', all.sum, 'Visitors')) %>%
        layout(title = 'All Visitation Counts Vs. Year', xaxis = list(title = 'Year'), yaxis = list(title = paste('All Visitation Counts')), 
               legend = l, autosize = F, width = 700, height = 700, margin = m)
}
     else if(input$season == 'summer') {
      plot_ly(summer, x = ~Year, y = ~ MR.sum, name = 'Mount Ranier National Park', type = 'scatter', mode = 'marker', hoverinfo = 'text',
              text = ~paste(Year,', ', MR.sum, 'Visitors')) %>%
        add_markers(y = ~ O.sum, name = 'Olypmic National Park', mode = 'marker', hoverinfo = 'text',
                    text = ~paste(Year,', ', O.sum, 'Visitors')) %>%
        add_markers(y = ~ LC.sum, name = 'Lake Chelan National Rec. Area', mode = 'marker', hoverinfo = 'text',
                    text = ~paste(Year,', ', LC.sum, 'Visitors')) %>%
        add_markers(y = ~ LR.sum, name = 'Lake Roosevelt National Rec. Area', mode = 'marker',hoverinfo = 'text',
                    text = ~paste(Year,', ', LR.sum, 'Visitors')) %>%
        add_markers(y = ~ NC.sum, name = 'North Cascades National Park', mode = 'marker', hoverinfo = 'text',
                    text = ~paste(Year,', ', NC.sum, 'Visitors')) %>%
        add_markers(y = ~ RL.sum, name = 'Ross Lake National Rec. Area', mode = 'marker', hoverinfo = 'text',
                    text = ~paste(Year,', ', RL.sum, 'Visitors')) %>%
        layout(title = 'Summer Visitation Count Vs. Year', xaxis = list(title = 'Year'), yaxis = list(title = paste('Summer Visitation Count')), 
               legend = l, autosize = F, width = 700, height = 700, margin = m)

}
    else if(input$season == 'winter') {
      plot_ly(winter, x = ~Year, y = ~ MR.sum, name = 'Mount Ranier National Park', type = 'scatter', mode = 'marker', hoverinfo = 'text',
              text = ~paste(Year,', ', MR.sum, 'Visitors')) %>%
        add_markers(y = ~ O.sum, name = 'Olypmic National Park', mode = 'marker', hoverinfo = 'text',
                    text = ~paste(Year,', ', O.sum, 'Visitors')) %>%
        add_markers(y = ~ LC.sum, name = 'Lake Chelan National Rec. Area', mode = 'marker', hoverinfo = 'text',
                    text = ~paste(Year,', ', LC.sum, 'Visitors')) %>%
        add_markers(y = ~ LR.sum, name = 'Lake Roosevelt National Rec. Area', mode = 'marker',hoverinfo = 'text',
                    text = ~paste(Year,', ', LR.sum, 'Visitors')) %>%
        add_markers(y = ~ NC.sum, name = 'North Cascades National Park', mode = 'marker', hoverinfo = 'text',
                    text = ~paste(Year,', ', NC.sum, 'Visitors')) %>%
        add_markers(y = ~ RL.sum, name = 'Ross Lake National Rec. Area', mode = 'marker', hoverinfo = 'text',
                    text = ~paste(Year,', ', RL.sum, 'Visitors')) %>%
        layout(title = 'Winter Visitation Count Vs. Year', xaxis = list(title = 'Year'), yaxis = list(title = paste('Winter Visitation Count')),
               legend = l, autosize = F, width = 700, height = 700, margin = m)
    } else if(input$season == 'spring') {
      plot_ly(spring, x = ~Year, y = ~ MR.sum, name = 'Mount Ranier National Park', type = 'scatter', mode = 'marker', hoverinfo = 'text',
              text = ~paste(Year,', ', MR.sum, 'Visitors')) %>%
        add_markers(y = ~ O.sum, name = 'Olypmic National Park', mode = 'marker', hoverinfo = 'text',
                    text = ~paste(Year,', ', O.sum, 'Visitors')) %>%
        add_markers(y = ~ LC.sum, name = 'Lake Chelan National Rec. Area', mode = 'marker', hoverinfo = 'text',
                    text = ~paste(Year,', ', LC.sum, 'Visitors')) %>%
        add_markers(y = ~ LR.sum, name = 'Lake Roosevelt National Rec. Area', mode = 'marker',hoverinfo = 'text',
                    text = ~paste(Year,', ', LR.sum, 'Visitors')) %>%
        add_markers(y = ~ NC.sum, name = 'North Cascades National Park', mode = 'marker', hoverinfo = 'text',
                    text = ~paste(Year,', ', NC.sum, 'Visitors')) %>%
        add_markers(y = ~ RL.sum, name = 'Ross Lake National Rec. Area', mode = 'marker', hoverinfo = 'text',
                    text = ~paste(Year,', ', RL.sum, 'Visitors')) %>%
        layout(title = 'Spring Visitation Count Vs. Year', xaxis = list(title = 'Year'), yaxis = list(title = paste('Spring Visitation Count')), 
               legend = l, autosize = F, width = 700, height = 700, margin = m)
    } else if(input$season == 'fall') {
      plot_ly(fall, x = ~Year, y = ~ MR.sum, name = 'Mount Ranier National Park', type = 'scatter', mode = 'marker', hoverinfo = 'text',
               text = ~paste(Year,', ', MR.sum, 'Visitors')) %>%
        add_markers(y = ~ O.sum, name = 'Olypmic National Park', mode = 'marker', hoverinfo = 'text',
                    text = ~paste(Year,', ', O.sum, 'Visitors')) %>%
        add_markers(y = ~ LC.sum, name = 'Lake Chelan National Rec. Area', mode = 'marker', hoverinfo = 'text',
                    text = ~paste(Year,', ', LC.sum, 'Visitors')) %>%
        add_markers(y = ~ LR.sum, name = 'Lake Roosevelt National Rec. Area', mode = 'marker',hoverinfo = 'text',
                    text = ~paste(Year,', ', LR.sum, 'Visitors')) %>%
        add_markers(y = ~ NC.sum, name = 'North Cascades National Park', mode = 'marker', hoverinfo = 'text',
                    text = ~paste(Year,', ', NC.sum, 'Visitors')) %>%
        add_markers(y = ~ RL.sum, name = 'Ross Lake National Rec. Area', mode = 'marker', hoverinfo = 'text',
                    text = ~paste(Year,', ', RL.sum, 'Visitors')) %>%
        layout(title = 'Fall Visitation Count Vs. Year', xaxis = list(title = 'Year'), yaxis = list(title = paste('Fall Visitation Count')), 
               legend = l, autosize = F, width = 700, height = 700, margin = m)
    }
    
  })
})
