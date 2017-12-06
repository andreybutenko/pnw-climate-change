library(plyr)
library(dplyr)
library(raster)
library(rgdal)
library(sp)
library(rgeos)
select <- dplyr::select # overwrite raster library

source('./scripts/hydro-helper.R')

Contains <- function(string, search) {
  return(grepl(search, string))
}

GetFileName <- function(string) {
  string <- string %>%
    strsplit('/') %>%
    sapply(function(v) { return(v[length(v)]) })
  string <- string %>%
    strsplit('[.]asc') %>% 
    sapply(function(v) { return(v[1]) })
  return(string)
}

# Get all paths in directory ----

base.path <- './data/hydroclimate-scenarios/'
directories <- paste0(base.path, list.files(base.path), '/monthly_summaries/')
paths <- lapply(directories, function(directory) {
  return(paste0(directory, list.files(directory)))
})

paths <- do.call(c, paths)

# Get data and metadata from each ----

data <- lapply(paths, function(path) {
  file.name <- GetFileName(path)
  print(path)
  
  year <- file.name %>% 
    strsplit('[.]') %>% 
    sapply(function(v) { return(v[2]) }) %>% 
    strsplit('-') %>% 
    sapply(function(v) { return(v[1]) })
  
  month <- file.name %>% 
    strsplit('[.]') %>% 
    sapply(function(v) { return(v[1]) }) %>% 
    strsplit('_') %>% 
    sapply(function(v) { return(v[length(v)]) })
  
  if(Contains(path, 'B1')) {
    scenario <- 'B1'
  }
  if(Contains(path, 'A1B')) {
    scenario <- 'A1B'
  }
  if(Contains(path, '1948-2000')) {
    scenario <- 'historic'
  }
  
  measure <- file.name %>% 
    strsplit('_') %>% 
    sapply(function(v) { return(v[1]) })
  
  data <- ImportAsc(path) %>% 
    cbind(list(
      year = year,
      month = month,
      scenario = scenario,
      measure = measure
    ))
  
  return(data)
})

combined.data <- do.call(rbind, data) %>% 
  select(year, month, scenario, measure, x, y, value)

# Filter down and save!

combined.data %>% 
  FilterToRegion(include.oregon = T) %>% 
  write.csv(file = './data/hydroclimate-scenarios/hydro-combined.csv', row.names = F)
