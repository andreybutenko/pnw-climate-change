# install.packages('devtools')
# install.packages('rgdal')
# devtools::install_github('dkahle/ggmap')

library(dplyr)
library(plyr)
library(rgdal)
library(raster)
library(sp)
library(rgeos)

ImportAsc <- function(path) {
  asc.data <- raster(path)
  result <- as.data.frame(asc.data, xy = T)
  colnames(result) <- c('x', 'y', 'value')
  result <- filter(result, !is.na(value))
  return(result)
}

ImportShp <- function(path) {
  # https://www.r-bloggers.com/r-and-gis-working-with-shapefiles/
  shp.data <- readOGR(path)
  
  crs <- CRS('+init=epsg:4326') # CSR reference: https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf
  shp.trans <- spTransform(x, crs) # transform to lat/long
  
  shp.df <- data.frame(x.trans) %>% 
    # rename_('lat' = 'coords.x2', 'long' = 'coords.x1')
    rename_('x' = 'coords.x1', 'y' = 'coords.x2')
  
  return(shp.df)
}

FilterToRegion <- function(df, include.oregon = F) {
  result <- df %>% 
    mutate(x = x - 360) %>% # offset to right longitude
    filter(x > -125, x  <  -117, y < 49, y > 42) %>% # exclude everything outside of WA and OR
    filter(x > -122.5 | y < 48) # exclude NW extents
  
  if(!include.oregon) {
    result <- result %>%
      filter(x > -123 | y > 46) %>% # exclude SW extents
      filter(x < -120 | y > 46) %>% # exclude SE extents
      filter(y > 45.5) # exclude Oregon
  }
  
  return(result)
}
DataMethods <- list(
  Subtract = function(a, b) {
    a$value <- a$value - b$value
    return(a)
  },
  Add = function(a, b) {
    a$value <- a$value + b$value
    return(a)
  },
  Divide = function(a, k) {
    a$value <- a$value / k
    return(a)
  },
  TripleAverage = function(a, b, c) {
    a$value <- (a$value + b$value + c$value) / 3
    return(a)
  }
)

GetDataPath <- function(scenario, measure, month, years, dataset = 'BCSD_hadcm', prefix = '../data/hydroclimate-scenarios/', historic = F) {
  if(!historic) {
    return(paste0(prefix, dataset, '_', scenario, '/monthly_summaries/', measure, '_', month, '.', years, '.asc'))
  } else {
    return(paste0(prefix, '1948-2000.Pcorr55N_Pref_Tref/monthly_summaries/', measure, '_', month, '.1950-2000.asc'))
  }
}

GetSeasonalAverage <- function(scenario, measure, season, years, dataset = 'BCSD_hadcm', historic = F, include.oregon = F) {
  months <- c(
    'dec', 'jan', 'feb',
    'mar', 'apr', 'may',
    'jun', 'jul', 'aug',
    'sep', 'oct', 'nov'
  )
  
  GetDataForIndex <- function(index) {
    GetDataPath(scenario, measure, months[index], years, historic = historic) %>% 
      ImportAsc() %>% 
      FilterToRegion(include.oregon = include.oregon) %>% 
      return()
  }
  
  season.offset <- mapvalues(
    season,
    from = c('winter', 'spring', 'summer', 'fall'),
    to = c(1, 4, 7, 10)
  ) %>%
    as.numeric()
  
  result <- DataMethods$TripleAverage(
    GetDataForIndex(season.offset + 0),
    GetDataForIndex(season.offset + 1),
    GetDataForIndex(season.offset + 2)
  )
  
  return(result)
}

GetMonthlyData <- function(scenario, measure, years, dataset = 'BCSD_hadcm', historic = F, include.oregon = F, name = F) {
  months <- c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
  
  GetDataForIndex <- function(month) {
    GetDataPath(scenario, measure, month, years, historic = historic) %>% 
      ImportAsc() %>% 
      FilterToRegion(include.oregon = include.oregon) %>% 
      return()
  }
  
  res <- lapply(months, GetDataForIndex)
  
  if(name) {
    names(res) <- months
  }
  
  return(res)
}