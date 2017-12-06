# Setup
library(dplyr)

avg.change.raw <- read.csv("../data/GCM-avg-change.csv", stringsAsFactors = F)
time.evolv.raw <- read.csv("../data/GCM-TEP.csv", stringsAsFactors = F)

# Finding appropriate model
te.models <- unique(time.evolv.proj$Model)
ac.models <- unique(avg.change.proj$`Model Name`)
models.in.both <- ac.models %in% te.models
model.names <- ac.models[models.in.both]
model.chosen <- "GFDL-ESM2G"

# Average Change Projections Clean Up
avg.change.proj <- avg.change.raw[3:length(avg.change.raw$X),]
colnames(avg.change.proj) <- avg.change.raw[2,]
avg.change.proj$Season <- recode(avg.change.proj$Season, 
                                 "ANN" = "Annual",
                                 "DJF" = "Winter",
                                 "MAM" = "Spring",
                                 "JJA" = "Summer",
                                 "SON" = "Autumn")
avg.change.proj$Variable <- recode(avg.change.proj$Variable,
                                   "tas" = "Near-Surface Air Temperature",
                                   "pr" = "Precipitation")
avg.change.proj$Scenario <- recode(avg.change.proj$Scenario,
                                   "rcp26" = "RCP2.6", 
                                   "rcp45" = "RCP4.5", 
                                   "rcp60" = "RCP6", 
                                   "rcp85" = "RCP8.5")
avg.change.proj <- avg.change.proj %>% 
  filter(`Projected Change` != "NaN") %>% 
  filter(`Model Name` == model.chosen)

# Split average change into two datasets
avg.change.temp <- avg.change.proj %>% 
  filter(Variable == "Near-Surface Air Temperature")
avg.change.prec <- avg.change.proj %>% 
  filter(Variable == "Precipitation")

# Time Evolving Projections Clean Up
time.evolv.proj <- time.evolv.raw
colnames(time.evolv.proj) <- c("Variable", "Epoch", "Scenario", "Season", "Model", "Year", "Value")
time.evolv.proj$Scenario <- recode(time.evolv.proj$Scenario,
                                   "rcp26" = "RCP2.6", 
                                   "rcp45" = "RCP4.5", 
                                   "rcp60" = "RCP6", 
                                   "rcp85" = "RCP8.5")
time.evolv.proj$Variable <- recode(time.evolv.proj$Variable,
                                   "tas" = "Near-Surface Air Temperature",
                                   "pr" = "Precipitation")
time.evolv.proj$Season <- recode(time.evolv.proj$Season, 
                                 "ANN" = "Annual",
                                 "DJF" = "Winter",
                                 "MAM" = "Spring",
                                 "JJA" = "Summer",
                                 "SON" = "Autumn")
time.evolv.proj$Epoch <- recode(time.evolv.proj$Epoch,
                                "historical" = "Historical",
                                "future" = "Future")
time.evolv.proj <- time.evolv.proj %>% 
  filter(Model == model.chosen) %>% 
  select(-Model)

# Split time evolving projections into two datasets
time.evolv.temp <- time.evolv.proj %>% 
  filter(Variable == "Near-Surface Air Temperature") %>% 
  select(-Variable) %>% 
  group_by(Year = trunc((Year/10))*10, Epoch, Season, Scenario) %>% 
  summarise(mean(Value))
time.evolv.prec <- time.evolv.proj %>% 
  filter(Variable == "Precipitation") %>% 
  select(-Variable) %>% 
  group_by(Year = trunc((Year/10))*10, Epoch, Season, Scenario) %>% 
  summarise(mean(Value))

# Create new data Files to use
write.csv(avg.change.prec, "../data/global-climate-models/GCM-avg-precipitation.csv")
write.csv(avg.change.temp, "../data/global-climate-models/GCM-avg-temp.csv")
write.csv(time.evolv.prec, "../data/global-climate-models/GCM-TEP-precipitation.csv")
write.csv(time.evolv.temp, "../data/global-climate-models/GCM-TEP-temp.csv")
