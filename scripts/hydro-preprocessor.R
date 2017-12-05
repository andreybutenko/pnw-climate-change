library(plyr)
library(dplyr)
library(raster)
library(rgdal)
library(sp)
library(rgeos)
select <- dplyr::select # overwrite raster library

source('./scripts/hydro-helper.R')

# Define all permutations ----

measures <- c('baseflow_monthly_tot', 'isr_monthly_avg', 'olr_monthly_avg', 'precip_monthly_tot',
              'runoff_monthly_tot', 'snodep_monthly_day1', 'swe_monthly_day1', 'tavg_monthly_avg')

months <- c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')

years <- c('historic', '2030-2059', '2070-2099')

scenarios <- c('A1B', 'B1')

# Functions to support permutations ----

## Import every month ----
ImportAllFilesForMeasureYear <- function(scenario, measure, year, historic = F) {
  data <- months %>% 
    lapply(function(month) {
      measure.root <- measure %>%
        strsplit('_') %>%
        sapply(function(v) { return(v[1]) })
      
      year.root <- year %>%
        strsplit('-') %>%
        sapply(function(v) { return(v[1]) })
      
      GetDataPath(scenario, measure, month, year, historic = historic) %>% 
        ImportAsc() %>% 
        cbind(list(
          measure = measure.root,
          month = month,
          scenario = if(!historic) scenario else 'historic',
          year = if(!historic) year.root else 'historic'
        ))
    })
  
  do.call(rbind, data) %>% 
    return()
}

## Import every year ----
ImportAllFilesForMeasure <- function(scenario, measure) {
  data <- years %>% 
    lapply(function(year) {
      ImportAllFilesForMeasureYear(scenario, measure, year, historic = year == 'historic')
    })
  
  do.call(rbind, data) %>%
    return()
}

## Import every measure ----
ImportAllFilesForScenario <- function(scenario) {
  data <- measures %>% 
    lapply(function(measure) {
      ImportAllFilesForMeasure(scenario, measure) %>% 
        return()
    })
  
  do.call(rbind, data) %>%
    return()
}

## Import every scenario ----
ImportAllFiles <- function() {
  data <- scenarios %>% 
    lapply(ImportAllFilesForScenario)
  
  do.call(rbind, data) %>% 
    select(year, month, scenario, measure, x, y, value) %>% 
    return()
}

# Save Data ----

data <- ImportAllFiles()
write.csv(data, file = './data/hydroclimate-scenarios/hydro-combined.csv', row.names = F)

# data %>% filter(year == 2030, scenario == 'A1B', measure == 'baseflow') %>% nrow # 109644
# GetDataPath('A1B', 'baseflow_monthly_tot', month, year, historic = historic) %>% 
#   ImportAsc() %>% 
