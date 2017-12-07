library(dplyr)
library(ggplot2)

wildfire.data <- read.csv('./data/wildfires/olympic-wildfires.csv')
names(wildfire.data) <- c('year', 'month', 'name', 'class', 'cause')

PlotWildfireChart <- function(years, causes, classes, group.by = 'month') {
  chart.data <- wildfire.data %>% 
    filter(year %in% years, cause %in% causes, class %in% classes) %>% 
    group_by_(group.by) %>% 
    dplyr::summarize(value = n()) %>% 
    as.data.frame()
  
  if(group.by == 'month') {
    months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
    chart.data$month <- factor(chart.data$month, levels = months) # keep month order when plotting
  }
  
  names(chart.data)[1] <- 'unit.time'

  ggplot(data = chart.data) +
    geom_line(
      mapping = aes(
        x = unit.time,
        y = value,
        group = 1
      ),
      size = 2
    ) +
    ggtitle('Wildfires over Time', subtitle = 'Olympic National Park') +
    xlab(group.by) +
    ylab('Number of Wildfires') %>% 
    return()
}