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
  'baseflow_monthly_tot' = 'Baseflow (inches)',
  'precip_monthly_tot' = 'Precipitation (inches)',
  'runoff_monthly_tot' = 'Runoff (inches)',
  'swe_monthly_day1' = 'Snowpack (inches)',
  'tavg_monthly' = 'Avg Temperature (degC)'
)

chart.titles <- list(
  'baseflow_monthly_tot' = 'Baseflow by',
  'precip_monthly_tot' = 'Precipitaton by',
  'runoff_monthly_tot' = 'Runoff by',
  'swe_monthly_day1' = 'Snowpack by',
  'tavg_monthly' = 'Temperature by'
)

HydroMonthlyPlot <- function(measure, year) {
  ProcessData <- function(df) {
    df <- df %>% 
      sapply(function(df) {
        return(mean(df$value))
      }) %>% 
      as.data.frame() %>%
      tibble::rownames_to_column()
    
    colnames(df) <- c('month', 'value')
    df$month <- factor(df$month, levels = df$month) # maintain order when plotting
    
    return(df)
  }
  
  historic.data <- GetMonthlyData(measure = measure, name = T, historic = T) %>% ProcessData()
  a1b.data <- GetMonthlyData('A1B', measure, year, name = T) %>% ProcessData()
  b1.data <- GetMonthlyData('B1', measure, year, name = T) %>% ProcessData()
  
  combined <- rbind(
    cbind(historic.data, list(category = 'historic')),
    cbind(a1b.data, list(category = 'a1b')),
    cbind(b1.data, list(category = 'b1'))
  )
  
  chart <- ggplot() +
    geom_line(
      data = combined,
      mapping = aes(x = month, y = value, color = category, group = category),
      size = 2
    ) +
    scale_colour_manual(
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
    xlab('Month') +
    ylab(axis.labels[[measure]]) +
    ggtitle(paste(chart.titles[[measure]], 'Month'))
  
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

# HydroMonthlyPlot('swe_monthly_day1', '2070-2099')