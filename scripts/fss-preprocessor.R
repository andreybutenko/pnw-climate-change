library(dplyr)

source('./hydro-helper.R')

# Import data sets ----

data.set.names <- c('MidColumbia', 'MiddleSnake', 'OregonCoast', 'SouthCentralOregon', 'Spokoot', 'UpperColumbiaYakima', 'WACoast')

GetDataSet <- function(name) {
  path <- paste0('../data/fss-stream-temp/', name, '/NorWeST_PredictedStreamTempPoints_', name, '.shp')
  df <- ImportShp(path)
  return(df)
}

data.set.data <- lapply(data.set.names, GetDataSet)
names(data.set.data) <- data.set.names



# Save each data set as csv ----
# It's faster to load in CSVs than Shapefiles, so this is a good restore point

SaveIntermediateDataSet <- function(name) {
  path <- paste0('../data/fss-stream-temp/intermediates/', name, '.csv')
  write.csv(data.set.data[[name]], file = path, row.names = FALSE)
  return(path)
}

lapply(data.set.names, SaveIntermediateDataSet)



# Cut down to relevant columns ----

SelectRelevantColumns <- function(name) {
  # Some of the datasets have inconsistent column names
  # The scenario number is always correct, so remove labels after the _
  name <- 'WACoast'
  names(data.set.data[[name]]) <- names(data.set.data[[name]]) %>%
    strsplit('_') %>%
    sapply(function(v) { return(v[1]) })
  
  # Documentation on columns provided
  # https://www.fs.fed.us/rm/boise/AWAE/projects/NorWeST/downloads/NorWeST_HistoricalStreamTempScenarioDescriptions.pdf
  columns <- c('x', 'y', 'ELEV', 'PRECIP', 'S1', 'S23', 'S25', 'S27', 'S29', 'S31', 'S36', 'S38', 'S40')
  
  result <- data.set.data[[name]] %>%
    select(one_of(columns)) %>%
    rename(
      x = x,
      y = y,
      elev = ELEV,
      precip = PRECIP,
      avg.1993.2011 = S1,
      add.1c = S23,
      add.2c = S25,
      add.3c = S27,
      a1b.2030.2059 = S29,
      a1b.2070.2099 = S31,
      aug.2015 = S36,
      mwmt.a1b.2030.2059 = S38,
      mwmt.a1b.2070.2099 = S40
    )
  
  return(result)
}

data.set.data <- lapply(data.set.names, SelectRelevantColumns)



# Combine dataframes and save ----

data.set.combined <- do.call(rbind, data.set.data)
write.csv(data.set.combined, file = '../data/fss-stream-temp/stream-temps-combined.csv', row.names = FALSE)
