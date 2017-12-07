library(dplyr)
library(ggplot2)

wildfire.data <- read.csv('./data/wildfires/olympic-wildfires.csv')
names(wildfire.data) <- c('year', 'month', 'name', 'class', 'cause')

PlotChart <- function() {
  chart.data <- wildfire.data %>% 
    group_by('month') %>% 
    summarize(value = n()) %>% 
    as.data.frame()
  
  months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  chart.data$month <- factor(chart.data$month, levels = months) # keep month order when plotting

  ggplot(data = chart.data) +
    geom_line(
      mapping = aes(
        x = month,
        y = value
      )
    ) %>% 
    return()
}
