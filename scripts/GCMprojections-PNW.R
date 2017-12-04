# Setup
library(dplyr)

avg.change.raw <- read.csv("../data/GCM-avg-change.csv", stringsAsFactors = F)
time.evolv.raw <- read.csv("../data/GCM-TEP.csv", stringsAsFactors = F)

# Average Change Projections Clean Up
avg.change.proj <- avg.change.raw[3:length(avg.change.raw$X),]
colnames(avg.change.proj) <- avg.change.raw[2,]

