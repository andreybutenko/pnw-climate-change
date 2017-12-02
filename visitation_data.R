#Libraries
library(dplyr)
library(stringr)

#Setting working directory

setwd("~/pnw-climate-change")

#Reads in Visitation data
mt.ranier.data.set <- read.csv('./data/Mount_Ranier_Visitation.csv', sep = ',', stringsAsFactors = FALSE)
olympic.data.set <- read.csv('./data/Olympic_Visitation.csv', sep = ',', stringsAsFactors = FALSE)
lake.chelan.data.set <- read.csv('./data/Lake_Chelan_Visitation.csv', sep = ',', stringsAsFactors = FALSE)
lake.roosevelt.data.set <- read.csv('./data/Lake_Roosevelt_Visitation.csv', sep = ',', stringsAsFactors = FALSE)
north.cascades.data.set <- read.csv('./data/North_Cascades_Visitation.csv', sep = ',', stringsAsFactors = FALSE)
ross.lake.data.set <- read.csv('./data/Ross_Lake_Visitation.csv', sep = ',', stringsAsFactors = FALSE)


#Changes colnames to prep for joins
colnames(mt.ranier.data.set) <- paste("MR", colnames(mt.ranier.data.set), sep = '_')
colnames(mt.ranier.data.set)[1] <- 'Year'
colnames(olympic.data.set) <- paste('O', colnames(olympic.data.set), sep = '_')
colnames(olympic.data.set)[1] <- 'Year'
colnames(lake.chelan.data.set) <- paste('LC', colnames(lake.chelan.data.set), sep = '_')
colnames(lake.chelan.data.set)[1] <- 'Year'
colnames(lake.roosevelt.data.set) <- paste('LR', colnames(lake.roosevelt.data.set), sep = '_')
colnames(lake.roosevelt.data.set)[1] <- 'Year'
colnames(north.cascades.data.set) <- paste('NC', colnames(north.cascades.data.set), sep = '_')
colnames(north.cascades.data.set)[1] <- 'Year'
colnames(ross.lake.data.set) <- paste('RL', colnames(ross.lake.data.set), sep = '_')
colnames(ross.lake.data.set)[1] <- 'Year'

#full join data
all.data <- full_join(mt.ranier.data.set, olympic.data.set, by = 'Year')
all.data <- full_join(all.data, lake.chelan.data.set, by = 'Year')
all.data <- full_join(all.data, lake.chelan.data.set, by = 'Year')
all.data <- full_join(all.data, north.cascades.data.set, by = 'Year')
all.data <- full_join(all.data, ross.lake.data.set, by = 'Year')

summer <- select(all.data, Year, contains('JUN'), contains('JUL'), contains('AUG'))
fall <- select(all.data, Year, contains('SEP'), contains('OCT'), contains('NOV'))
winter <- select(all.data, Year, contains('DEC'), contains('JAN'), contains('FEB'))
spring <- select(all.data, Year, contains('MAR'), contains('APR'), contains('MAY'))