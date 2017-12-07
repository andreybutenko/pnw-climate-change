# install.packages('devtools')
# install.packages('rgdal')
# devtools::install_github('dkahle/ggmap')

library(plyr)
library(dplyr)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(sp)
library(rgeos)
# Uncomment these when re-importing data sets (with preprocessors)
# library(raster)
# library(rgdal)

select <- dplyr::select # overwrite raster library
summarize <- dplyr::summarize # overwrite plyr library

# Import data

hydro.data <- read.csv('./data/hydroclimate-scenarios/hydro-combined.csv', stringsAsFactors = F)

# Seasons

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

# Geographic Chart

MapPNWData <- function(df, column = 'value',
                       color.low = '#cccccc', color.mid = '#cccccc', color.high = 'blue',
                       title = 'Chart', subtitle = NULL, color.legend.title = 'value',
                       long.range = c(-125, -116), lat.range = c(42, 49),
                       offset.long = 0,
                       point.size = 5, include.oregon = F) {
  pnw.map <- if(include.oregon) {
    fortify(map_data('state', region = c('washington', 'oregon')))
  } else {
    fortify(map_data('state', region = c('washington')))
  }
  
  map.data <- df
  
  plot <- ggplot(data = map.data) +
    geom_map(
      data = pnw.map,
      map = pnw.map,
      mapping = aes(x = long, y = lat, map_id = region),
      fill = '#ffffff'
    ) +
    geom_point(
      mapping = aes(
        x = x + offset.long,
        y = y,
        size = map.data[,column],
        color = map.data[,column]
      ),
      size = point.size
    ) +
    scale_colour_gradient2(
      low = color.low,
      high = color.high,
      mid = color.mid
    ) +
    ggtitle(title, subtitle = subtitle) +
    guides(
      color = guide_colorbar(color.legend.title)
    ) +
    coord_fixed(
      ratio = 1.3,
      xlim = long.range,
      ylim = if(include.oregon) { lat.range } else { c(45.5, 49) }
    )
  
  return(plot)
}

# Helpers for hydroclimate scenarios dataset ----

GetDataPath <- function(scenario, measure, month, years, dataset = 'BCSD_hadcm', prefix = './data/hydroclimate-scenarios/', historic = F) {
  if(!historic) {
    return(paste0(prefix, dataset, '_', scenario, '/monthly_summaries/', measure, '_', month, '.', years, '.asc'))
  } else {
    return(paste0(prefix, '1948-2000.Pcorr55N_Pref_Tref/monthly_summaries/', measure, '_', month, '.1950-2000.asc'))
  }
}

ImportAsc <- function(path) {
  asc.data <- raster::raster(path)
  result <- as.data.frame(asc.data, xy = T)
  colnames(result) <- c('x', 'y', 'value')
  result <- filter(result, !is.na(value))
  return(result)
}

# Helpers for stream temps data set ----

ImportShp <- function(path) {
  # https://www.r-bloggers.com/r-and-gis-working-with-shapefiles/
  shp.data <- readOGR(path)
  
  crs <- CRS('+init=epsg:4326') # CSR reference: https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf
  shp.trans <- spTransform(shp.data, crs) # transform to lat/long
  
  shp.df <- data.frame(shp.trans) %>% 
    # rename_('lat' = 'coords.x2', 'long' = 'coords.x1')
    rename_('x' = 'coords.x1', 'y' = 'coords.x2')
  
  return(shp.df)
}



# General helpers ----

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

GetSeasonalAverage <- function(scenario.req, measure.req, season, year.req, include.oregon = F) {
  months <- c(
    'dec', 'jan', 'feb',
    'mar', 'apr', 'may',
    'jun', 'jul', 'aug',
    'sep', 'oct', 'nov'
  )
  
  GetDataForIndex <- function(index) {
    hydro.data %>% 
      filter(scenario == scenario.req, measure == measure.req, month == months[index], year == year.req) %>%  
      return()
  }
  
  season.offset <- mapvalues(
    season,
    from = c('winter', 'spring', 'summer', 'fall'),
    to = c(1, 4, 7, 10)
  ) %>%
    as.numeric()

  result <- rbind(
    GetDataForIndex(season.offset + 0),
    GetDataForIndex(season.offset + 1),
    GetDataForIndex(season.offset + 2)
  ) %>%
    group_by(.dots = c('x', 'y')) %>% 
    summarize(value = mean(value)) %>% 
    as.data.frame()

  return(result)
}

GetMonthlyData <- function(scenario.req, measure.req, year.req, include.oregon = F) {
  hydro.data %>% 
    filter(scenario == scenario.req, measure == measure.req, year == year.req) %>% 
    return()
}
