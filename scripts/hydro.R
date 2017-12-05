library(dplyr)
library(ggplot2)
library(ggmap)
library(gridExtra)

source('./scripts/hydro-helper.R')

# Show how runoff varies seasonally ----

winter.runoff.historic.chart <- MapPNWData(
  GetSeasonalAverage(measure = 'runoff_monthly_tot', season = 'winter', historic = T, include.oregon = T),
  title = 'Avg Monthly Runoff, Winter',
  subtitle = '1950-2000',
  color.legend.title = 'Runoff (inches)',
  include.oregon = T
)

summer.runoff.historic.chart <- MapPNWData(
  GetSeasonalAverage(measure = 'runoff_monthly_tot', season = 'summer', historic = T, include.oregon = T),
  title = 'Avg Monthly Runoff, Summer',
  subtitle = '1950-2000',
  color.legend.title = 'Runoff (inches)',
  include.oregon = T
)



# Show seasonal cycle of snowpack ----

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

snowpack.combined <- rbind(
  cbind(snowpack.monthly.historic, list(category = 'historic')),
  cbind(snowpack.monthly.b1.2070, list(category = 'b1.2070')),
  cbind(snowpack.monthly.a1b.2070, list(category = 'a1b.2070'))
)


snowpack.changes <- ggplot() +
  geom_line(
    data = snowpack.combined,
    mapping = aes(x = month, y = value, color = category, group = category),
    size = 2
  ) +
  scale_colour_manual(
    name = 'Scenario',
    values = c(
      historic = 'black',
      b1.2070 = 'blue',
      a1b.2070 = 'red'
    ),
    labels = c(
      historic = 'Avg 1950-2000',
      b1.2070 = 'Lower emissions,\nAvg 2070-2099',
      a1b.2070 = 'Higher emissions,\nAvg 2070-2099'
    )
  ) +
  xlab('Month') +
  ylab('Snowpack (inches)') +
  ggtitle('Projected monthly snowpack depth')



# Show how snowpack is related to runoff ----

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

snowpack.runoff.historic.combined <- rbind(
  cbind(snowpack.monthly.historic, list(category = 'snowpack.historic')),
  cbind(runoff.monthly.historic, list(category = 'runoff.historic'))
)

snowpack.vs.runoff <- ggplot() +
  geom_line(
    data = snowpack.runoff.historic.combined,
    mapping = aes(x = month, y = value, color = category, group = category),
    size = 2
  ) +
  scale_colour_manual(
    name = 'Value',
    values = c(
      snowpack.historic = 'black',
      runoff.historic = 'blue'
    ),
    labels = c(
      snowpack.historic = 'Snowpack',
      runoff.historic = 'Runoff'
    )
  ) +
  xlab('Month') +
  ylab('Value (inches)') +
  ggtitle('Snowpack vs Runoff', subtitle = 'Avg 1950-2000')


# Show how summer runoff will change in future ----

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
  color.legend.title = 'Changes in runoff (inches)',
  point.size = 8
)



# Show how emissions scenarios affect seasonal runoff ----

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


# grid.arrange(winter.runoff.diff.2070.chart, summer.runoff.diff.2070.chart, ncol = 2)



# Show how snowpack will change in the future ----


