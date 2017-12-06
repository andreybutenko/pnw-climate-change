library(dplyr)

source('./scripts/hydro-helper.R')

# Import data sets ----

data.set.names <- c('MidColumbia', 'MiddleSnake', 'OregonCoast', 'SouthCentralOregon', 'Spokoot', 'UpperColumbiaYakima', 'WACoast')

GetDataSet <- function(name) {
  path <- paste0('./data/fss-stream-temp/', name, '/NorWeST_PredictedStreamTempPoints_', name, '.shp')
  df <- ImportShp(path)
  return(df)
}

data.set.data <- lapply(data.set.names, GetDataSet)
names(data.set.data) <- data.set.names


# Add row label to data sets

AddLabel <- function(name) {
  data.set.data[[name]]$label <- name
  return(data.set.data[[name]])
}

data.set.data <- lapply(data.set.names, AddLabel)
names(data.set.data) <- data.set.names


# Save each data set as csv ----
# It's faster to load in CSVs than Shapefiles, so this is a good restore point

SaveIntermediateDataSet <- function(name) {
  path <- paste0('./data/fss-stream-temp/intermediates/', name, '.csv')
  write.csv(data.set.data[[name]], file = path, row.names = FALSE)
  return(path)
}

lapply(data.set.names, SaveIntermediateDataSet)
names(data.set.data) <- data.set.names


# Restore, if necessary ----
# Comment out the ones without S40

data.set.data <- list(
  read.csv('./data/fss-stream-temp/intermediates/MidColumbia.csv', stringsAsFactors = F),
  #read.csv('./data/fss-stream-temp/intermediates/MiddleSnake.csv', stringsAsFactors = F),
  read.csv('./data/fss-stream-temp/intermediates/OregonCoast.csv', stringsAsFactors = F),
  #read.csv('./data/fss-stream-temp/intermediates/SouthCentralOregon.csv', stringsAsFactors = F),
  #read.csv('./data/fss-stream-temp/intermediates/Spokoot.csv', stringsAsFactors = F),
  read.csv('./data/fss-stream-temp/intermediates/UpperColumbiaYakima.csv', stringsAsFactors = F),
  read.csv('./data/fss-stream-temp/intermediates/WACoast.csv', stringsAsFactors = F)
)
data.set.reduced <- c('MidColumbia', 'OregonCoast', 'UpperColumbiaYakima', 'WACoast')
names(data.set.data) <- data.set.reduced


# Cut down to relevant columns ----

SelectRelevantColumns <- function(name) {
  # Some of the datasets have inconsistent column names
  # The scenario number is always correct, so remove labels after the _
  print(name)
  print(names(data.set.data[[name]]))
  names(data.set.data[[name]]) <- names(data.set.data[[name]]) %>%
    strsplit('_') %>%
    sapply(function(v) { return(v[1]) })
  
  # Documentation on columns provided
  # https://www.fs.fed.us/rm/boise/AWAE/projects/NorWeST/downloads/NorWeST_HistoricalStreamTempScenarioDescriptions.pdf
  columns <- c('x', 'y', 'S1', 'S40', 'label')
  
  result <- data.set.data[[name]] %>%
    select(one_of(columns)) %>%
    rename(
      x = x,
      y = y,
      # elev = ELEV,
      # precip = PRECIP,
      avg.1993.2011 = S1,
      # add.1c = S23,
      # add.2c = S25,
      # add.3c = S27,
      # a1b.2030.2059 = S29,
      # a1b.2070.2099 = S31,
      # aug.2015 = S36,
      # mwmt.a1b.2030.2059 = S38,
      mwmt.a1b.2070.2099 = S40
    )
  
  return(result)
}

data.set.data <- lapply(data.set.reduced, SelectRelevantColumns)
names(data.set.data) <- data.set.reduced



# Combine dataframes and save ----

data.set.combined <- do.call(rbind, data.set.data)
write.csv(data.set.combined, file = './data/fss-stream-temp/stream-temps-combined.csv', row.names = FALSE)
names(data.set.data) <- data.set.reduced



# Get even smaller subsets for individual charts ----

## Intro ----

data.set.combined %>% 
  filter(x > -125, x < -122, y < 49, y > 46.5) %>% 
  select(x, y, avg.1993.2011, mwmt.a1b.2070.2099) %>% 
  filter(avg.1993.2011 != -9999, mwmt.a1b.2070.2099 != -9999) %>% 
  write.csv(file = './data/fss-stream-temp/stream-temp-oly.csv')

## Suitable, future ----

wa.subset <- data.set.combined %>% 
  select(x, y, mwmt.a1b.2070.2099) %>% 
  filter(x > -125, x  <  -117, y < 49, y > 42) %>% # exclude everything outside of WA and OR
  filter(x > -122.5 | y < 48) %>% # exclude NW extents
  filter(x > -123 | y > 46) %>% # exclude SW extents
  filter(x < -120 | y > 46) %>% # exclude SE extents
  filter(y > 45.5) %>% # exclude Oregon
  filter(mwmt.a1b.2070.2099 != -9999) %>% 
  rename(value = mwmt.a1b.2070.2099)

GetAverageInGrid <- function(long.min, long.max, lat.min, lat.max) {
  data <- wa.subset %>% 
    filter(x > long.min, x < long.max, y > lat.min, y < lat.max)
  mid.x <- (long.min + long.max) / 2
  mid.y <- (lat.min + lat.max) / 2
  avg <- mean(data$value)
  data.frame(x = mid.x, y = mid.y, value = avg) %>% 
    return()
}

average.grid <- data.frame(x = c(), y = c(), value = c())

for(long in seq(
  from = min(wa.subset$x),
  to = max(wa.subset$x),
  by = (max(wa.subset$x) - min(wa.subset$x)) / 20
)) {
  for(lat in seq(
    from = min(wa.subset$y),
    to = max(wa.subset$y),
    by = (max(wa.subset$y) - min(wa.subset$y)) / 15
  )) {
    average.grid <- rbind(
      average.grid,
      GetAverageInGrid(
        long,
        long + ((max(wa.subset$x) - min(wa.subset$x)) / 20),
        lat,
        lat + ((max(wa.subset$y) - min(wa.subset$y)) / 15)
      )
    )
  }
}

average.grid %>% 
  filter(value != 0) %>% 
  write.csv(file = './data/fss-stream-temp/stream-temp-grid.csv')


# Whale! ----

# for(long in seq(
#   from = min(wa.subset$x),
#   to = max(wa.subset$x),
#   by = (max(wa.subset$x) - min(wa.subset$x)) / 15
# )) {
#   for(lat in seq(
#     from = min(wa.subset$y),
#     to = max(wa.subset$y),
#     by = (max(wa.subset$y) - min(wa.subset$y)) / 10
#   )) {
#     average.grid <- rbind(
#       average.grid,
#       GetAverageInGrid(
#         long,
#         long + ((max(wa.subset$x) - min(wa.subset$x)) / 10),
#         lat,
#         lat + ((max(wa.subset$y) - min(wa.subset$y)) / 10)
#       )
#     )
#   }
# }
# MapPNWData(average.grid %>% filter(value != 0), point.size = 2)
