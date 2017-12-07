#After #7 and #8, we need to show the different metrics we have available:

#Number of employed people
#Percent of total economy
#Gross revenues
#We have this data down to a county-level, so probably want to show how it affects people at a very local level.

library(gdata) 
library(dplyr)
library(stringr)
library(plotly)
library(gdata)
library(knitr)
library(ggmap)
library(rgeos)
library(ggplot2)
library(devtools)
library(maps)
library(mapdata)
library(scales)

employment.data <- read.csv("./data/ENOW_NES/ENOW_NES_2005_2014.csv", stringsAsFactors = FALSE)

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
# 
# total.economy.wa <- summarize(washington.state.data,
#                               sum = sum(washington.state.data$GrossReceipts)
#                               )
# 
# states <- map_data("state")
# Washington <- subset(states, region %in% c("washington"))
# 
# wa_df <- subset(states, region == "washington")
# counties <- map_data("county")
# wa_county <- subset(counties, region == "washington")


#midpoints of the counties to plot the county information 
# county.midpoints <- wa_county %>% 
#   rename(x = long, y = lat) %>% 
#   group_by(subregion) %>% 
#   summarize(x = mean(x), y = mean(y)) %>% 
#   as.data.frame()
#   write.csv(county.midpoints, file = "./data/ENOW_NES/CountyMidpoints.csv", row.names = F)
#   colnames(county.midpoints)[which(names(county.midpoints) == "subregion")] <- ""
  
county.midpoints <- read.csv('./data/ENOW_NES/CountyMidpoints.csv', stringsAsFactors = F)
county.midpoints <- rename(county.midpoints, GeoName = subregion)

# washington.county.data %>% View
washington.county.data$GeoName <- lapply(washington.county.data$GeoName %>% 
  strsplit(' '), function(v) { return(str_to_lower(v[1]) )}) %>% unlist()

# gross.receipts.mean <- washington.county.data %>% 
#                               group_by(GeoName) %>% 
#                               summarize(avg = mean(GrossReceipts))  
#   
# gross.receipts.mean.county <- left_join(county.midpoints, gross.receipts.mean) %>% 
#                                 filter(!is.na(avg))
#                               
# MapPNWData(gross.receipts.mean.county, column = 'avg', long.range = c(-125, -121), lat.range = c(45.5, 49), include.oregon = T)

MapEmploymentData <- function(years, column, industries, operation) {
  chart.data.prelim <- washington.county.data %>% 
    filter(Year %in% years, OceanSector %in% industries) %>% 
    select_('GeoName', column)
  if(operation == 'mean' & column == 'Employment') {
    chart.data <- chart.data.prelim %>% 
      group_by(GeoName) %>% 
      summarize(value = mean(Employment))
  } else if(operation == 'mean' & column == 'GrossReceipts') {
    chart.data <- chart.data.prelim %>% 
      group_by(GeoName) %>% 
      summarize(value = mean(GrossReceipts))
  } else if(operation == 'sum' & column == 'Employment') {
    chart.data <- chart.data.prelim %>% 
      group_by(GeoName) %>% 
      summarize(value = sum(Employment))
  } else if(operation == 'sum' & column == 'GrossReceipts') {
    chart.data <- chart.data.prelim %>% 
      group_by(GeoName) %>% 
      summarize(value = sum (GrossReceipts))
  } 
  chart.data <- left_join(county.midpoints, chart.data) %>% 
                                filter(!is.na(value))
  MapGeoData(chart.data, column = 'value', long.range = c(-125, -121), lat.range = c(45.5, 49), include.oregon = T, title = 'Role of Ocean Resources in Economy by County', color.legend.title = 'Amount ($)')
}


#function mapping out Western WA impacted by the climate change

MapGeoData <- function(df, column = 'value',
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
    scale_colour_continuous(
      label = comma
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







# 
# map.details <- list(
#   scope = "Washington",
#   projection = list(type = "albers usa"),
#   showland = TRUE,
#   landcolor = toRGB("gray55"),
#   subunitwidth = 1,
#   countrywidth = 1,
#   subunitcolor = toRGB("white"),
#   countrycolor = toRGB("white")
# )
# pointer.details <- plot_geo(data, lat = ~lat, lon = ~lng, locationmode = 'US A-states') %>%
#   add_markers(
#     hoverinfo = "text", text = ~paste(data$date, data$city, paste("Injured:", data$injured), paste("Killed:", data$killed), sep = "<br />"),
#     color = ~(injured+killed), colors = 'Reds', symbol = I("circle"), size = ~(data$injured + data$killed), alpha = 1.0
#   ) %>%
#   colorbar(title = "All affected<br />casualties") %>%
#   layout(
#     title = 'Shootings in USA<br />(Put mouse on the dots for details)', geo = map.details
#   )
# 
# 
# shinyUI(my.ui)
# 
# 
# 
# function (year, county)
#   filterstate of where year == year 
#   





