#After #7 and #8, we need to show the different metrics we have available:

#Number of employed people
#Percent of total economy
#Gross revenues
#We have this data down to a county-level, so probably want to show how it affects people at a very local level.

library(gdata) 
library(dplyr)
library(stringr)
library(plotly)

help("read.csv")

getwd()

setwd("~/Desktop/info201/final/pnw-climate-change/data/ENOW_NES/")
employment.data <- read.csv("ENOW_NES_2005_2014.csv", stringsAsFactors = FALSE)

#data set of the washingston state 
washington.state.data <- employment.data %>% 
                    filter(str_detect(employment.data$GeoName, "Washington")) %>% 
                    filter(str_detect(GeoScale, "State"))

#retrieved the name of the counties in WA on the website as the data set itself doesn't 
#clarify which county is in the washington state. 
county.names <- c("Clallam County", 
                  "Grays Harbor County", "Island County", "Jefferson County",
                  "King County", "Kitsap County", "Mason County", "Pacific County", 
                  "Pierce County", "San Juan County", "Skagit County", "Snohomish County", "Thurston County",
                  "Wahkiakum County", "Whatcom County") 

#data set of the counties in the washington state.
washington.county.data <- employment.data %>% 
                          filter(employment.data$GeoName %in% county.names) %>% 
                          filter(!str_detect(Employment, '-9999'))







