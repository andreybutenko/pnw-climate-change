library(dplyr)
library(ggplot2)
library(ggmap)
library(gridExtra)

source('./scripts/hydro-helper.R')

fss.data <- read.csv('./data/fss-stream-temp/stream-temps-combined.csv', stringsAsFactors = F)

fss.data %>% 
  MapPNWData(column = 'add.1c')
