library(dplyr)
library(ggplot2)
library(ggmap)
library(gridExtra)

source('./scripts/hydro-helper.R')

# fss.data <- read.csv('./data/fss-stream-temp/stream-temps-combined.csv', stringsAsFactors = F)
stream.temp.oly <- read.csv('./data/fss-stream-temp/stream-temp-oly.csv', stringsAsFactors = F)
stream.temp.grid <- read.csv('./data/fss-stream-temp/stream-temp-grid.csv', stringsAsFactors = F)

# Olympic Penninsula: Stream Temp Intro ----

future.stream.temp <- stream.temp.oly %>% 
  select(x, y, mwmt.a1b.2070.2099) %>% 
  rename(value = mwmt.a1b.2070.2099)

historic.stream.temp <- stream.temp.oly %>% 
  select(x, y, avg.1993.2011) %>% 
  rename(value = avg.1993.2011)

diff.stream.temp <- DataMethods$Subtract(future.stream.temp, historic.stream.temp)

future.stream.temp.chart <- future.stream.temp %>% 
  MapPNWData(
   long.range = c(-122.5, -124.75), lat.range = c(47, 48.4),
   include.oregon = T, color.high = 'red', color.mid = NULL, point.size = 0.5,
   title = 'Projected Stream Temperatures',
   subtitle = 'A1B Scenario, Avg 2070-2099',
   offset.long = 0.05, color.legend.title = 'Temperature (degC)'
 )

historic.stream.temp.chart <- historic.stream.temp %>% 
  MapPNWData(
    long.range = c(-122.5, -124.75), lat.range = c(47, 48.4),
    include.oregon = T, color.high = 'red', color.mid = NULL, point.size = 0.5,
    title = 'Historic Stream Temperatures',
    subtitle = 'Avg 1993-2011',
    offset.long = 0.05, color.legend.title = 'Temperature (degC)'
  )

diff.stream.temp.chart <- diff.stream.temp %>% 
  MapPNWData(
    long.range = c(-122.5, -124.75), lat.range = c(47, 48.4),
    include.oregon = T, color.high = 'red', color.mid = NULL, point.size = 0.5,
    title = 'Change in Stream Temperatures',
    subtitle = 'Between Avg 1993-2011 & Projected Avg 2070-2099',
    offset.long = 0.05, color.legend.title = 'Temperature (degC)'
  )

#grid.arrange(historic.stream.temp.chart, future.stream.temp.chart, ncol = 2)



# Washington State Temps ----

wa.stream.temp.chart <- MapPNWData(
  stream.temp.grid,
  color.high = 'red', color.mid = NULL, point.size = stream.temp.grid$value / 5,
  title = 'Washington State Stream Temperature',
  subtitle = 'A1B Scenario, Avg 2070-2099'
)



# Suitability Data ----

CategorizeTemp <- function(temp) {
  if(temp < 18) {
    return('good')
  } else if (temp >= 18 & temp < 22) {
    return('stressful')
  } else {
    return('deadly')
  }
}

suitability.data <- stream.temp.grid
suitability.data$suitability <- sapply(suitability.data$value, CategorizeTemp)

MapSuitabilityData <- function(df, column = 'suitability', title = 'Chart', subtitle = NULL) {
  pnw.map <- fortify(map_data('state', region = c('washington')))
  
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
      size = 4,
      show.legend = TRUE
    ) +
    scale_color_manual(
      name = 'Suitability',
      values = c(
        'good' = 'darkgreen',
        'stressful' = 'orange',
        'deadly' = 'red'
      )
    ) +
    ggtitle(title, subtitle = subtitle) +
    coord_fixed(
      ratio = 1.3,
      xlim = c(-125, -116),
      ylim = c(45.5, 49)
    )
  
  return(plot)
}

suitability.data.chart <- MapSuitabilityData(suitability.data, title = 'Suitability for Salmon Spawning', subtitle = 'A1B Scenario, Avg 2070-2099')
