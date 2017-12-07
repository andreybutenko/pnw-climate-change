library(dplyr)
library(stringr)
library(plotly)
library(tidyr)
library(ggplot2)
select <- dplyr::select 

#Setting working directory

setwd("~/pnw-climate-change")

#Reads in Visitation data
visitation.data <- rbind(
  read.csv('./data/park-visitation-data/Mount_Ranier_Visitation.csv', sep = ',', stringsAsFactors = FALSE) %>% mutate(Park = 'Mount Rainier'),
  read.csv('./data/park-visitation-data/Olympic_Visitation.csv', sep = ',', stringsAsFactors = FALSE) %>% mutate(Park = 'Olympic'),
  read.csv('./data/park-visitation-data/Lake_Chelan_Visitation.csv', sep = ',', stringsAsFactors = FALSE) %>% mutate(Park = 'Lake Chelan'),
  read.csv('./data/park-visitation-data/Lake_Roosevelt_Visitation.csv', sep = ',', stringsAsFactors = FALSE) %>% mutate(Park = 'Lake Roosevelt'),
  read.csv('./data/park-visitation-data/North_Cascades_Visitation.csv', sep = ',', stringsAsFactors = FALSE) %>% mutate(Park = 'North Cascades'),
  read.csv('./data/park-visitation-data/Ross_Lake_Visitation.csv', sep = ',', stringsAsFactors = FALSE) %>% mutate(Park = 'Ross Lake')
)


visitation.data <- gather(visitation.data, Year, Park, JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC)
names(visitation.data) <- c('year', 'park', 'month', 'visitors')

months <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')
visitation.data$month <- factor(visitation.data$month, levels = months) # keep month order when plotting

monthly.chart.data <- visitation.data %>% 
  group_by(.dots = c('park', 'month')) %>% 
  summarize(visitors = mean(visitors)) %>% 
  ungroup() 

#Creates the plot which shows average visitation by month from 1976 - 2016 
MonthPlot <- function() {
  #plotly style
  
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
  
    p <- plot_ly(monthly.chart.data, x = ~ month, y = ~visitors, color = ~park, type = 'scatter', mode = 'lines+markers', hoverinfo = 'text', text = paste(month,',', visitors, 'Visitors')) %>% 
      layout(title = 'Average Visitation per Month', xaxis = list(title = 'Year'), yaxis = list(title = paste('Winter Visitation Count')),
            legend = l, autosize = F, width = 700, height = 700, margin = m)
    return(p)
}

#Shows the visitation of annual visitation vs the Year. Takes in two parameters, the total count and the average along
# with the option to display a trend line.
AnnualVisitationPlot <- function(type, trend){
  if(type == 1){
    annual.visitaion <- visitation.data %>% 
      group_by(year) %>% 
      summarize(visitors = mean(visitors)) %>% 
      ungroup()
    if(trend == 2)
    {
      p <- plot_ly(annual.visitaion, x = ~ year, y = ~visitors, color = 'rgba(7, 164, 181, 1)', type = 'scatter', mode = 'lines+markers', hoverinfo = 'text', text = ~paste(year,',', visitors, 'Visitors')) %>%
        add_lines(y = ~fitted(loess(visitors ~ year)), line = list(color = '#07A4B5'), name = "Loess Smoother", showlegend = TRUE) %>%
        layout(title = 'Annual Visitation Mean Count Vs. Year', xaxis = list(title = 'Year'), yaxis = list(title = paste('Annual Visitation Count')),
               legend = l, autosize = F, width = 700, height = 700, margin = m)
      return(p)
    } else {
    p <- plot_ly(annual.visitaion, x = ~ year, y = ~visitors, color = 'rgba(7, 164, 181, 1)', type = 'scatter', mode = 'lines+markers', hoverinfo = 'text', text = ~paste(year,',', visitors, 'Visitors')) %>%
      layout(title = 'Annual Visitation Mean Count Vs. Year', xaxis = list(title = 'Year'), yaxis = list(title = paste('Annual Visitation Count')),
             legend = l, autosize = F, width = 700, height = 700, margin = m)
    return(p)
    }
  }
  else{
      annual.visitaion <- visitation.data %>% 
        group_by(year) %>% 
        summarize(visitors = sum(visitors)) %>% 
        ungroup()
      if(trend == 2){
        p <- plot_ly(annual.visitaion, x = ~ year, y = ~visitors, color = 'rgba(7, 164, 181, 1)', type = 'scatter', mode = 'lines+markers', hoverinfo = 'text', text = ~paste(year,',', visitors, 'Visitors')) %>%
          add_lines(y = ~fitted(loess(visitors ~ year)), line = list(color = '#07A4B5'), name = "Loess Smoother", showlegend = TRUE) %>%
          layout(title = 'Annual Visitation Total Count Vs. Year', xaxis = list(title = 'Year'), yaxis = list(title = paste('Annual Visitation Count')),
                 legend = l, autosize = F, width = 700, height = 700, margin = m)
    return(p)
      }
      else{
        p <- plot_ly(annual.visitaion, x = ~ year, y = ~visitors, color = 'rgba(7, 164, 181, 1)', type = 'scatter', mode = 'lines+markers',hoverinfo = 'text', text = ~paste(year,',', visitors, 'Visitors')) %>% 
          layout(title = 'Annual Visitation Total Count Vs. Year', xaxis = list(title = 'Year'), yaxis = list(title = paste('Annual Visitation Count')),
                 legend = l, autosize = F, width = 700, height = 700, margin = m)  
        return(p)
        }
      
  }
}

  PlotlyGraph <- function(season) {
    # plotly style and plot dimensions
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

 if(season == 'winter')
  {
    annual.visitaion <- visitation.data %>% filter(month == 'DEC', month == 'JAN', month == 'FEB')
    p <- plot_ly(annual.visitation, x = ~ year, y = ~visitors, color = ~park, type = 'scatter', mode = 'lines+markers', hoverinfo = 'text', text = ~paste(year,',', visitors, 'Visitors')) %>% 
      layout(title = 'Winter Visitation Count Vs. Year', xaxis = list(title = 'Year'), yaxis = list(title = paste('Winter Visitation Count')),
             legend = l, autosize = F, width = 700, height = 700, margin = m)  
    return(p)
  }
  else if(season == 'fall')
  {
    annual.visitaion <- visitation.data %>% filter(month == 'SEP', month == 'OCT', month == 'NOV')
    p <- plot_ly(annual.visitation, x = ~ year, y = ~visitors, color = ~park, type = 'scatter', mode = 'lines+markers', hoverinfo = 'text', text = ~paste(year,',', visitors, 'Visitors')) %>% 
      layout(title = 'Fall Visitation Count Vs. Year', xaxis = list(title = 'Year'), yaxis = list(title = paste('Fall Visitation Count')), 
             legend = l, autosize = F, width = 700, height = 700, margin = m)
    return(p)
  }
  else if(season == 'summer')
  {
    annual.visitaion <- visitation.data %>% filter(month == 'JUN', month == 'JUL', month == 'AUG')
    p <- plot_ly(annual.visitation, x = ~ year, y = ~visitors, color = ~park, type = 'scatter', mode = 'lines+markers', hoverinfo = 'text', text = ~paste(year,',', visitors, 'Visitors')) %>% 
      layout(title = 'Summer Visitation Count Vs. Year', xaxis = list(title = 'Year'), yaxis = list(title = paste('Summer Visitation Count')), 
             legend = l, autosize = F, width = 700, height = 700, margin = m)
    return(p)

  }
  else{
    annual.visitaion <- visitation.data %>% filter(month == 'MAR' , month == 'APR', month == 'MAY')
   
     p <- plot_ly(annual.visitation, x = ~ year, y = ~visitors, color = ~park, type = 'scatter', mode = 'lines+markers', hoverinfo = 'text', text = ~paste(year,',', visitors, 'Visitors')) %>%  
      layout(title = 'Spring Visitation Count Vs. Year', xaxis = list(title = 'Year'), yaxis = list(title = paste('Spring Visitation Count')), 
             legend = l, autosize = F, width = 700, height = 700, margin = m)

    return(p)
  }
    
}

