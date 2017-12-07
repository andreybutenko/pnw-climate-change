# Setup
library(dplyr)
library(ggplot2)

avg.change.prec <- read.csv("./data/global-climate-models/GCM-avg-precipitation.csv")
avg.change.temp <- read.csv("./data/global-climate-models/GCM-avg-temp.csv")
time.evolv.prec <- read.csv("./data/global-climate-models/GCM-TEP-precipitation.csv")
time.evolv.temp <- read.csv("./data/global-climate-models/GCM-TEP-temp.csv")

# Average Change Plot
MakeAvgChangePlot <- function(dataset, scenario, variable){
  if (variable == "Precipitation") {
    unit = "%"
  } else {
    unit = "degrees Celsius"
  }
  chart.data <- dataset %>% 
    filter(Scenario == scenario)
  avg.change.plot <- ggplot(chart.data, aes(x = Season,
                                 y = Projected.Change)) +
    geom_col(aes(fill = as.character(Time.Period))) +
    labs(y = paste0("Projected Change in ", variable, " in ", unit),
         title = paste0("Average Change in ", variable),
         fill = "Time Period")
  return(avg.change.plot)
}

# Time Evolving Plot
MakeTimeEvolvPlot <- function(dataset, season, variable) {
  if (variable == "Precipitation") {
    unit = "%"
  } else {
    unit = "degrees Celsius"
  }
  chart.data.post <- filter(dataset, Year >= 2000)
  chart.data.pre <- filter(dataset, Year <= 2000, Scenario == "RCP6")
  chart.data.pre$Scenario <- "Already Occurred"
  chart.data <- rbind(chart.data.pre, chart.data.post) %>% 
    filter(Season == season)
  time.evolv.plot <- ggplot(chart.data, 
                            aes(x = Year,
                                y = Value,
                                color = Scenario,
                                fill = Scenario)) +
    geom_line(alpha = .6) +
    geom_smooth(se = F, span = .4) +
    labs(y = paste0("Projected Change in ", variable, " in ", unit),
         title = paste0("Time Evolving Projections in ", variable))
  return(time.evolv.plot)
}
