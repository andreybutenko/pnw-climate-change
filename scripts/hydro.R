library(dplyr)
library(ggplot2)
library(ggmap)
library(gridExtra)

source('./hydro-helper.R')

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


