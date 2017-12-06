# Setup
library(dplyr)
library(ggplot2)

avg.change.prec <- read.csv("../data/global-climate-models/GCM-avg-precipitation.csv")
avg.change.temp <- read.csv("../data/global-climate-models/GCM-avg-temp.csv")
time.evolv.prec <- read.csv("../data/global-climate-models/GCM-TEP-precipitation.csv")
time.evolv.temp <- read.csv("../data/global-climate-models/GCM-TEP-temp.csv")

# Average Change in Precipitation Plot
MakeAvgChangePlot <- function(dataset, scenario, variable){
  if (variable == "Precipitation") {
    unit = "mm/s"
  } else {
    unit = "K"
  }
  chart.data <- dataset %>% 
    filter(Scenario == scenario)
  plot <- ggplot(chart.data, aes(x = Season,
                                 y = Projected.Change)) +
    geom_col(aes(fill = as.character(Time.Period))) +
    labs(y = paste0(" Projected Change in ", variable, " in ", unit),
         title = paste0("Average Change in ", variable),
         fill = "Time Period")
  return(plot)
}
