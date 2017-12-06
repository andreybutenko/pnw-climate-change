library(dplyr)
library(ggplot2)
library(ggmap)
library(gridExtra)

source('./scripts/hydro-helper.R')

fss.data <- read.csv('./data/fss-stream-temp/stream-temps-combined.csv', stringsAsFactors = F)
stream.temp.oly <- read.csv('./data/fss-stream-temp/stream-temp-oly.csv', stringsAsFactors = F)

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

# grid.arrange(historic.stream.temp, future.stream.temp, ncol = 2)


