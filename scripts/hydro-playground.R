library(dplyr)
library(ggplot2)
library(ggmap)
library(gridExtra)

source('./scripts/hydro-helper.R')

scenarios <- c(
  'Higher Emissions (A1B)' = 'A1B',
  'Lower Emissions (B1)' = 'B1',
  'Historic' = 'historic'
)

axis.labels <- list(
  'baseflow' = 'Baseflow (inches)',
  'precip' = 'Precipitation (inches)',
  'runoff' = 'Runoff (inches)',
  'swe' = 'Snowpack (inches)',
  'tavg' = 'Avg Temperature (degC)'
)

chart.titles <- list(
  'baseflow' = 'Baseflow by',
  'precip' = 'Precipitaton by',
  'runoff' = 'Runoff by',
  'swe' = 'Snowpack by',
  'tavg' = 'Temperature by'
)

HydroMonthlyPlot <- function(measure.req, year.req) {
  plot.data <- hydro.data %>% 
    filter(measure == measure.req, year == year.req | year == 1950) %>% 
    FilterToRegion() %>% 
    group_by(.dots = c('month', 'scenario')) %>% 
    summarize(value = mean(value)) %>% 
    ungroup() %>% 
    mutate(month = factor(month, levels = months))
  
  chart <- ggplot() +
    geom_line(
      data = plot.data,
      mapping = aes(x = month, y = value, color = scenario, group = scenario),
      size = 2
    ) +
    scale_colour_manual(
      name = 'Scenario',
      values = c(
        historic = 'black',
        B1 = 'blue',
        A1B = 'red'
      ),
      labels = c(
        historic = 'Avg 1950-2000',
        B1 = paste('Lower emissions,\nAvg', year.req),
        A1B = paste('Higher emissions,\nAvg', year.req)
      )
    ) +
    xlab('Month') +
    ylab(axis.labels[[measure.req]]) +
    ggtitle(paste(chart.titles[[measure.req]], 'Month'))
  
  return(chart)
}

HydroSeasonalChart <- function(measure.req, year.req) {
  GetSeason <- function(month) {
    seasons <- c('winter', 'spring', 'summer', 'fall')
    
    months <- c(
      'dec', 'jan', 'feb',
      'mar', 'apr', 'may',
      'jun', 'jul', 'aug',
      'sep', 'oct', 'nov'
    )
    
    return(seasons[ceiling(base::match(month, months) / 3)])
  }
  
  seasonal.data <- hydro.data %>%
    filter(measure == measure.req, year == 1950 | year  == year.req) %>% 
    FilterToRegion() %>% 
    mutate(season = GetSeason(month)) %>% 
    group_by(.dots = c('scenario', 'season')) %>% 
    summarize(value = mean(value))
  
  chart <- ggplot(data = seasonal.data) +
    geom_bar(
      mapping = aes(
        x = season,
        y = value,
        fill = scenario
      ),
      stat = 'identity',
      position = 'dodge'
    ) +
    scale_fill_manual(
      name = 'Scenario',
      values = c(
        historic = 'black',
        B1 = 'blue',
        A1B = 'red'
      ),
      labels = c(
        historic = 'Avg 1950-2000',
        B1 = paste('Lower emissions,\nAvg', year.req),
        A1B = paste('Higher emissions,\nAvg', year.req)
      )
    ) +
    xlab('Season') +
    ylab(axis.labels[[measure.req]]) +
    ggtitle(paste(chart.titles[[measure.req]], 'Season'))
  
  return(chart)
}
