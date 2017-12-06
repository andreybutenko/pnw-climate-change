library(dplyr)
library(stringr)
library(plotly)

select <- dplyr::select # Andrey's note: keep this here

#Setting working directory

#setwd("~/pnw-climate-change")

#Reads in Visitation data
visitation.data <- rbind(
  read.csv('./data/Mount_Ranier_Visitation.csv', sep = ',', stringsAsFactors = FALSE) %>% mutate(Park = 'Mount Rainier'),
  read.csv('./data/Olympic_Visitation.csv', sep = ',', stringsAsFactors = FALSE) %>% mutate(Park = 'Olympic'),
  read.csv('./data/Lake_Chelan_Visitation.csv', sep = ',', stringsAsFactors = FALSE) %>% mutate(Park = 'Lake Chelan'),
  read.csv('./data/Lake_Roosevelt_Visitation.csv', sep = ',', stringsAsFactors = FALSE) %>% mutate(Park = 'Lake Roosevelt'),
  read.csv('./data/North_Cascades_Visitation.csv', sep = ',', stringsAsFactors = FALSE) %>% mutate(Park = 'North Cascades'),
  read.csv('./data/Ross_Lake_Visitation.csv', sep = ',', stringsAsFactors = FALSE) %>% mutate(Park = 'Ross Lake')
)



# Andrey's Demo -----
library(tidyr)
library(ggplot2)

visitation.data <- gather(visitation.data, Year, Park, JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC)
names(visitation.data) <- c('year', 'park', 'month', 'visitors')

months <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')
visitation.data$month <- factor(visitation.data$month, levels = months) # keep month order when plotting

monthly.chart.data <- visitation.data %>% 
  group_by(.dots = c('park', 'month')) %>% 
  summarize(visitors = mean(visitors)) %>% 
  ungroup() 
month.graph <- function() {
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
  
    plot_ly(monthly.chart.data, x = ~ month, y = ~visitors, color = ~park, type = 'scatter', mode = 'lines+markers') %>% 
      layout(title = 'Average Visitation per Month', xaxis = list(title = 'Year'), yaxis = list(title = paste('Winter Visitation Count')),
            legend = l, autosize = F, width = 700, height = 700, margin = m)
}

PlotlyGraph <- function(season) {
  # plotly style 
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
   return(plot_ly(annual.visitation, x = ~ month, y = ~visitors, color = ~park, type = 'scatter', mode = 'lines+markers') %>% 
    layout(title = 'Winter Visitation Count Vs. Year', xaxis = list(title = 'Year'), yaxis = list(title = paste('Winter Visitation Count')),
                     legend = l, autosize = F, width = 700, height = 700, margin = m))
  }
  else if(season == 'fall')
  {
    annual.visitaion <- visitation.data %>% filter(month == 'SEP', month == 'OCT', month == 'NOV')
    return(plot_ly(annual.visitation, x = ~ month, y = ~visitors, color = ~park, type = 'scatter', mode = 'lines+markers') %>% 
    layout(title = 'Fall Visitation Count Vs. Year', xaxis = list(title = 'Year'), yaxis = list(title = paste('Fall Visitation Count')), 
                     legend = l, autosize = F, width = 700, height = 700, margin = m))
  }
  else if(season == 'summer')
  {
    annual.visitaion <- visitation.data %>% filter(month == 'JUN', month == 'JUL', month == 'AUG')
    
    return(plot_ly(annual.visitation, x = ~ month, y = ~visitors, color = ~park, type = 'scatter', mode = 'lines+markers') %>% 
    layout(title = 'Summer Visitation Count Vs. Year', xaxis = list(title = 'Year'), yaxis = list(title = paste('Summer Visitation Count')), 
                     legend = l, autosize = F, width = 700, height = 700, margin = m))

  }
  else if(season == 'spring') {
    annual.visitaion <- visitation.data %>% filter(month == 'MAR' , month == 'APR', month == 'MAY')
    
    return(plot_ly(annual.visitation, x = ~ month, y = ~visitors, color = ~park, type = 'scatter', mode = 'lines+markers') %>%  
    layout(title = 'Spring Visitation Count Vs. Year', xaxis = list(title = 'Year'), yaxis = list(title = paste('Spring Visitation Count')), 
                     legend = l, autosize = F, width = 700, height = 700, margin = m))
 
}
}
