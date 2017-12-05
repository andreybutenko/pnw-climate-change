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
  print(paste(measure.req, year.req))
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

HydroSeasonalChart <- function(measure, year) {
  seasons <- c('winter', 'spring', 'summer', 'fall')
  
  GetScenarioData <- function(scenario) {
    season.data <- lapply(seasons, function(season) {
      GetSeasonalAverage(scenario, 'precip_monthly_tot', season, '2070-2099', historic = scenario == 'historic') %>%
        (function(df) {
          return(mean(df$value))
        }) %>% 
        return()
    })
    
    data.frame(season = seasons, value = unlist(season.data)) %>% 
      return()
  }
  
  
  historic.data <- GetScenarioData('historic')
  a1b.data <- GetScenarioData('A1B')
  b1.data <- GetScenarioData('B1')
  
  combined <- rbind(
    cbind(historic.data, list(category = 'historic')),
    cbind(a1b.data, list(category = 'a1b')),
    cbind(b1.data, list(category = 'b1'))
  )
  
  ggplot(data = combined) +
    geom_bar(
      mapping = aes(
        x = season,
        y = value,
        fill = category
      ),
      stat = 'identity',
      position = 'dodge'
    ) +
    scale_fill_manual(
      name = 'Scenario',
      values = c(
        historic = 'black',
        b1 = 'blue',
        a1b = 'red'
      ),
      labels = c(
        historic = 'Avg 1950-2000',
        b1 = paste('Lower emissions,\nAvg', year),
        a1b = paste('Higher emissions,\nAvg', year)
      )
    ) +
    xlab('Season') +
    ylab(axis.labels[[measure]]) +
    ggtitle(paste(chart.titles[[measure]], 'Season'))
}