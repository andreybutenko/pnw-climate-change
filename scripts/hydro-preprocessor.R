library(dplyr)

source('./hydro-helper.R')

data.set.names <- c('MidColumbia', 'MiddleSnake', 'OregonCoast', 'SouthCentralOregon', 'Spokoot', 'UpperColumbiaYakima', 'WACoast')

GetDataSet <- function(name) {
  path <- paste0('../data/fss-stream-temp/', name, '/NorWeST_PredictedStreamTempPoints_', name, '.shp')
  df <- ImportShp(path)
  return(df)
}

SaveIntermediateDataSet <- function(name) {
  path <- paste0('../data/fss-stream-temp/intermediates/', name, '.csv')
  write.csv(data.set.data[[name]], file = path, row.names = FALSE)
  return(path)
}

data.set.data <- lapply(data.set.names, GetDataSet)
names(data.set.data) <- data.set.names

lapply(data.set.names, SaveIntermediateDataSet)

