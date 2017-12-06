library(dplyr)
library(ggplot2)
library(ggmap)
library(gridExtra)

source('./scripts/hydro-helper.R')

# Show how runoff varies seasonally ----

winter.runoff.historic.chart <- MapPNWData(
  GetSeasonalAverage('historic', 'runoff', 'winter', 1950, include.oregon = T),
  title = 'Avg Monthly Runoff, Winter',
  subtitle = '1950-2000',
  color.legend.title = 'Runoff (inches)',
  include.oregon = T
)

summer.runoff.historic.chart <- MapPNWData(
  GetSeasonalAverage('historic', 'runoff', 'summer', 1950, include.oregon = T),
  title = 'Avg Monthly Runoff, Summer',
  subtitle = '1950-2000',
  color.legend.title = 'Runoff (inches)',
  include.oregon = T
)



# Show seasonal cycle of snowpack ----

months <- c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')

snowpack.data <- hydro.data %>% 
  filter(measure == 'swe', year == 2070 | year == 1950) %>%
  FilterToRegion() %>% 
  group_by(.dots = c('month', 'scenario')) %>% 
  summarize(value = mean(value)) %>% 
  ungroup() %>% 
  mutate(month = factor(month, levels = months))

snowpack.changes <- ggplot() +
  geom_line(
    data = snowpack.data,
    mapping = aes(x = month, y = value, color = scenario, group = scenario),
    size = 2
  ) +
  scale_colour_manual(
    name = 'Scenario',
    values = c(
      historic = 'black',
      B1 = 'blue',
      A1B = 'red'
    ),
    labels = c(
      historic = 'Avg 1950-2000',
      B1 = 'Lower emissions,\nAvg 2070-2099',
      A1B = 'Higher emissions,\nAvg 2070-2099'
    )
  ) +
  xlab('Month') +
  ylab('Snowpack (inches)') +
  ggtitle('Projected monthly snowpack depth')


# Show how snowpack is related to runoff ----

snowpack.runoff.data <- hydro.data %>% 
  filter(scenario == 'historic', measure == 'swe' | measure == 'runoff') %>% 
  FilterToRegion() %>% 
  group_by(.dots = c('month', 'measure')) %>% 
  summarize(value = mean(value)) %>% 
  ungroup() %>% 
  mutate(month = factor(month, levels = months))

snowpack.vs.runoff <- ggplot() +
  geom_line(
    data = snowpack.runoff.data,
    mapping = aes(x = month, y = value, color = measure, group = measure),
    size = 2
  ) +
  scale_colour_manual(
    name = 'Value',
    values = c(
      swe = 'black',
      runoff = 'blue'
    ),
    labels = c(
      swe = 'Snowpack',
      runoff = 'Runoff'
    )
  ) +
  xlab('Month') +
  ylab('Value (inches)') +
  ggtitle('Snowpack vs Runoff', subtitle = 'Avg 1950-2000')


# Show how summer runoff will change in future ----

summer.runoff.historic <- GetSeasonalAverage('historic', 'runoff', 'summer', 1950)

summer.runoff.a1b.2070 <- GetSeasonalAverage('A1B', 'runoff', 'summer', 2070)

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

winter.runoff.a1b.2070 <- GetSeasonalAverage('A1B', 'runoff', 'winter', 2070)

winter.runoff.b1.2070 <- GetSeasonalAverage('B1', 'runoff', 'winter', 2070)

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



# Show how snowpack will change in the future ----


