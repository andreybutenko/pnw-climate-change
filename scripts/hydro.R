# install.packages('maps')
# install.packages('mapdata')
# install.packages('devtools')
# install.packages('maptools')
# devtools::install_github('dkahle/ggmap')

library(dplyr)
library(plyr)
library(raster)
library(rgdal)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(maptools)
library(mapproj)
library(gridExtra)

ImportAsc <- function(path) {
  asc.data <- raster(path)
  result <- as.data.frame(asc.data, xy = T)
  colnames(result) <- c('x', 'y', 'value')
  result <- filter(result, !is.na(value))
  return(result)
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

MapPNWData <- function(df, column = 'value', color.low = '#cccccc', color.mid = '#cccccc', color.high = 'blue', title = 'Chart', subtitle = NULL, color.legend.title = 'value', point.size = 5, include.oregon = F) {
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
        x = x,
        y = y,
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
      xlim = c(-125, -116),
      ylim = if(include.oregon) { c(42, 49) } else { c(45.5, 49) }
    )
  
  return(plot)
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


### Show how runoff varies seasonally

winter.runoff.historic.chart <- MapPNWData(
  GetSeasonalAverage(measure = 'runoff_monthly_tot', season = 'winter', historic = T, include.oregon = T),
  title = 'Avg Monthly Runoff, Winter 1950-2000',
  include.oregon = T
)

summer.runoff.historic.chart <- MapPNWData(
  GetSeasonalAverage(measure = 'runoff_monthly_tot', season = 'summer', historic = T, include.oregon = T),
  title = 'Avg Monthly Runoff, Summer 1950-2000',
  include.oregon = T
)

grid.arrange(winter.runoff.historic.chart, summer.runoff.historic.chart, ncol = 2)



### Show seasonal cycle of snowpack

snowpack.monthly.historic <- GetMonthlyData(measure = 'swe_monthly_day1', historic = T, name = T) %>% 
  sapply(function(df) {
    return(mean(df$value))
  }) %>% 
  as.data.frame() %>%
  tibble::rownames_to_column()

colnames(snowpack.monthly.historic) <- c('month', 'value')
snowpack.monthly.historic$month <- factor( # convert to factor to maintain order when plotting
  snowpack.monthly.historic$month,
  levels = snowpack.monthly.historic$month
)


snowpack.monthly.a1b.2070 <- GetMonthlyData('A1B', 'swe_monthly_day1','2070-2099', name = T) %>% 
  sapply(function(df) {
    return(mean(df$value))
  }) %>% 
  as.data.frame() %>%
  tibble::rownames_to_column()

colnames(snowpack.monthly.a1b.2070) <- c('month', 'value')
snowpack.monthly.a1b.2070$month <- factor( # convert to factor to maintain order when plotting
  snowpack.monthly.a1b.2070$month,
  levels = snowpack.monthly.a1b.2070$month
)


snowpack.monthly.b1.2070 <- GetMonthlyData('B1', 'swe_monthly_day1','2070-2099', name = T) %>% 
  sapply(function(df) {
    return(mean(df$value))
  }) %>% 
  as.data.frame() %>%
  tibble::rownames_to_column()

colnames(snowpack.monthly.b1.2070) <- c('month', 'value')
snowpack.monthly.b1.2070$month <- factor( # convert to factor to maintain order when plotting
  snowpack.monthly.b1.2070$month,
  levels = snowpack.monthly.b1.2070$month
)


ggplot() +
  geom_line(
    data = snowpack.monthly.historic,
    mapping = aes(x = month, y = value, group = 1),
    color = 'black',
    size = 2
  ) +
  geom_line(
    data = snowpack.monthly.a1b.2070,
    mapping = aes(x = month, y = value, group = 1),
    color = 'red',
    size = 2
  ) +
  geom_line(
    data = snowpack.monthly.b1.2070,
    mapping = aes(x = month, y = value, group = 1),
    color = 'blue',
    size = 2
  )



### Show how snowpack is related to runoff

snowpack.monthly.historic <- GetMonthlyData(measure = 'swe_monthly_day1', historic = T, name = T) %>% 
  sapply(function(df) {
    return(mean(df$value))
  }) %>% 
  as.data.frame() %>%
  tibble::rownames_to_column()

colnames(snowpack.monthly.historic) <- c('month', 'value')
snowpack.monthly.historic$month <- factor( # convert to factor to maintain order when plotting
  snowpack.monthly.historic$month,
  levels = snowpack.monthly.historic$month
)


runoff.monthly.historic <- GetMonthlyData(measure = 'runoff_monthly_tot', historic = T, name = T) %>% 
  sapply(function(df) {
    return(mean(df$value))
  }) %>% 
  as.data.frame() %>%
  tibble::rownames_to_column()

colnames(runoff.monthly.historic) <- c('month', 'value')
runoff.monthly.historic$month <- factor( # convert to factor to maintain order when plotting
  runoff.monthly.historic$month,
  levels = runoff.monthly.historic$month
)

ggplot() +
  geom_line(
    data = snowpack.monthly.historic,
    mapping = aes(x = month, y = value, group = 1),
    color = 'black',
    size = 2
  ) +
  geom_line(
    data = runoff.monthly.historic,
    mapping = aes(x = month, y = value, group = 1),
    color = 'blue',
    size = 2
  )


### Show how summer runoff will change in future

summer.runoff.historic <- GetSeasonalAverage(measure = 'runoff_monthly_tot', season = 'summer', historic = T)

summer.runoff.a1b.2070 <- GetSeasonalAverage('A1B', 'runoff_monthly_tot', 'summer', '2070-2099')

summer.runoff.diff.historic <- DataMethods$Subtract(summer.runoff.a1b.2070, summer.runoff.historic)

summer.runoff.diff.historic.chart <- MapPNWData(
  summer.runoff.diff.historic,
  title = 'Projected Avg Change in Runoff, Summer',
  subtitle = 'Red indicates reduced runoff in future.\nComparing 1950-2000 & higher-emissions 2070-2099 scenario.',
  color.low = 'red',
  color.mid = 'gray',
  color.high = 'blue',
  point.size = 8
)

summer.runoff.diff.historic.chart



### Show how emissions scenarios affect runoff

winter.runoff.a1b.2070 <- GetSeasonalAverage('A1B', 'runoff_monthly_tot', 'winter', '2070-2099')

winter.runoff.b1.2070 <- GetSeasonalAverage('B1', 'runoff_monthly_tot', 'winter', '2070-2099')

winter.runoff.diff.2070 <- DataMethods$Subtract(winter.runoff.a1b.2070, winter.runoff.b1.2070)

winter.runoff.diff.2070.chart <- MapPNWData(
  winter.runoff.diff.2070,
  title = 'Avg Change in Winter Runoff Between Scenarios',
  subtitle = 'Red indicates reduced runoff in higher-emissions scenario, 2070-2099',
  color.low = 'red',
  color.mid = 'gray',
  color.high = 'blue',
  point.size = 4
)


summer.runoff.a1b.2070 <- GetSeasonalAverage('A1B', 'runoff_monthly_tot', 'summer', '2070-2099')

summer.runoff.b1.2070 <- GetSeasonalAverage('B1', 'runoff_monthly_tot', 'summer', '2070-2099')

summer.runoff.diff.2070 <- DataMethods$Subtract(summer.runoff.a1b.2070, summer.runoff.b1.2070)

summer.runoff.diff.2070.chart <- MapPNWData(
  summer.runoff.diff.2070,
  title = 'Avg Change in Summer Runoff Between Scenarios',
  subtitle = 'Red indicates reduced runoff in higher-emissions scenario, 2070-2099',
  color.low = 'red',
  color.mid = 'gray',
  color.high = 'blue',
  point.size = 4
)


grid.arrange(winter.runoff.diff.2070.chart, summer.runoff.diff.2070.chart, ncol = 2)



### Show how snowpack will change in the future


# OLD EXPERIMENTS

# plot(x)
# image(x)
# 
# 
# x <- raster('./BCSD_hadcm_A1B/monthly_summaries/precip_monthly_tot_apr.2030-2059.asc')
# x <- raster('./BCSD_hadcm_A1B/monthly_summaries/tavg_monthly_avg_apr.2030-2059.asc')
# 
# df <- as.data.frame(x, xy = T)
# df <- df %>% 
#   filter()
# View(df)
# 
# ggplot(data = pnw.map) +
#   geom_point(
#     mapping = aes(
#       x = long,
#       y = lat
#     )
#   )
# 
# plot_geo(
#   x = ~baseflow.apr.2030.2059.a1b.df$x,
#   y = ~baseflow.apr.2030.2059.a1b.df$y
# ) %>% 
#   add_markers(
#     color = ~baseflow.apr.2030.2059.a1b.df$baseflow_monthly_tot_apr.2030.2059
#   ) %>% 
#   layout(
#     geo = list(
#       scope = 'usa',
#       center = list(
#         long = 47.3865308,
#         lat = -120.5238015
#       )
#     )
#   )
