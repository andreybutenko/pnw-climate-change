#Libraries
library(dplyr)
library(stringr)
library(plotly)

select <- dplyr::select # Andrey's note: keep this here

#Setting working directory

#setwd("~/pnw-climate-change")

#Reads in Visitation data
visitation.data <- rbind(
  read.csv('./data/Mount_Ranier_Visitation.csv', sep = ',', stringsAsFactors = FALSE) %>% mutate(Park = 'Mount Rainier'),
  read.csv('./data/Olympic_Visitation.csv', sep = ',', stringsAsFactors = FALSE) %>% mutate(Park = 'Olympic'),
  read.csv('./data/Lake_Chelan_Visitation.csv', sep = ',', stringsAsFactors = FALSE) %>% mutate(Park = 'Lake Chelan'),
  read.csv('./data/Lake_Roosevelt_Visitation.csv', sep = ',', stringsAsFactors = FALSE) %>% mutate(Park = 'Lake Roosevelt'),
  read.csv('./data/North_Cascades_Visitation.csv', sep = ',', stringsAsFactors = FALSE) %>% mutate(Park = 'North Cascades'),
  read.csv('./data/Ross_Lake_Visitation.csv', sep = ',', stringsAsFactors = FALSE) %>% mutate(Park = 'Ross Lake')
)

# Andrey's Demo -----
library(tidyr)
library(ggplot2)

visitation.data <- gather(visitation.data, Year, Park, JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC)
names(visitation.data) <- c('year', 'park', 'month', 'visitors')

months <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')
visitation.data$month <- factor(visitation.data$month, levels = months) # keep month order when plotting

monthly.chart.data <- visitation.data %>% 
  group_by(.dots = c('park', 'month')) %>% 
  summarize(visitors = mean(visitors)) %>% 
  ungroup() 

ggplot(data = monthly.chart.data) +
  geom_line(mapping = aes(x = month, y = visitors, color = park, group = park, size = 2 ))

##################

mt.ranier.data.set <- mt.ranier.data.set %>% mutate(Park = 'Mount Ranier National Park')
olympic.data.set <- olympic.data.set %>% mutate(Park = 'Olympic National Park')
lake.chelan.data.set <- lake.chelan.data.set %>% mutate(Park = 'Lake Chelan Rec. Area')
lake.roosevelt.data.set <- lake.roosevelt.data.set %>% mutate(Park = 'Lake Roosevelt Rec. Area')
north.cascades.data.set <- north.cascades.data.set %>% mutate(Park = 'North Cascades National Park')
ross.lake.data.set <- ross.lake.data.set %>% mutate(Park = 'Ross Lake Rec. Area')
 
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
all.data <- full_join(all.data, lake.roosevelt.data.set, by = 'Year')
all.data <- full_join(all.data, north.cascades.data.set, by = 'Year')
all.data <- full_join(all.data, ross.lake.data.set, by = 'Year')

#filters data
summer <- all.data %>%  select(Year, contains('JUN'), contains('JUL'), contains('AUG')) %>% group_by()
fall <- select(all.data, Year, contains('SEP'), contains('OCT'), contains('NOV'))
winter <- select(all.data, Year, contains('DEC'), contains('JAN'), contains('FEB'))
spring <- select(all.data, Year, contains('MAR'), contains('APR'), contains('MAY'))

# ParkSeasonSum <- function(season) {
# season <- season
# if(season == 'winter')
# {
#   a = 'DEC'
#   b = 'JAN'
#   c = 'FEB'
# }
# else if(season == 'fall')
# {
#   a = 'SEP'
#   b = 'OCT'
#   c = 'NOV'
# }
# else if(season == 'summer')
# {
#   a = 'JUN'
#   b = 'JUL'
#   c = 'AUG'
# }
# else {
#   a = 'MAR'
#   b = 'APR'
#   c = 'MAY'
# }
#}

#Creates new columns that has the sum of the nat parks' visitors for that season
summer <- summer %>% mutate(MR.sum = MR_JUN + MR_JUL + MR_AUG)
summer <- summer %>% mutate(O.sum = O_JUN + O_JUL + O_AUG)
summer <- summer %>% mutate(LC.sum = LC_JUN + LC_JUL + LC_AUG)
summer <- summer %>% mutate(LR.sum = LR_JUN + LC_JUL + LC_AUG)
summer <- summer %>% mutate(NC.sum = LR_JUN +LC_JUL + LC_AUG)
summer <- summer %>% mutate(RL.sum = RL_JUN + RL_JUL + RL_AUG)
summer <- summer %>% mutate(all.sum = MR.sum + O.sum + LC.sum + LR.sum + NC.sum + RL.sum)

winter <- winter %>% mutate(MR.sum = MR_DEC + MR_JAN + MR_FEB)
winter <- winter %>% mutate(O.sum = O_DEC + O_JAN + O_FEB)
winter <- winter %>% mutate(LC.sum = LC_DEC + LC_JAN + LC_FEB)
winter <- winter %>% mutate(LR.sum = LC_DEC + LC_JAN + LC_FEB)
winter <- winter %>% mutate(NC.sum = NC_DEC + NC_JAN + LC_FEB)
winter <- winter %>% mutate(RL.sum = RL_DEC + RL_JAN + RL_FEB)
winter <- winter %>% mutate(all.sum = MR.sum + O.sum + LC.sum + LR.sum + NC.sum + RL.sum)

spring <- spring %>% mutate(MR.sum = MR_MAR + MR_APR + MR_MAY)
spring <- spring %>% mutate(O.sum = O_MAR + O_APR + O_MAY)
spring <- spring %>% mutate(LC.sum = LC_MAR + LC_APR + LC_MAY)
spring <- spring %>% mutate(LR.sum = LC_MAR + LC_APR + LC_MAY)
spring <- spring %>% mutate(NC.sum = NC_MAR + NC_APR + LC_MAY)
spring <- spring %>% mutate(RL.sum = RL_MAR + RL_APR + RL_MAY)
spring <- spring %>% mutate(all.sum = MR.sum + O.sum + LC.sum + LR.sum + NC.sum + RL.sum)

fall <- fall %>% mutate(MR.sum = MR_SEP + MR_OCT + MR_NOV)
fall <- fall %>% mutate(O.sum = O_SEP + O_OCT + O_NOV)
fall <- fall %>% mutate(LC.sum = LC_SEP + LC_OCT + LC_NOV)
fall <- fall %>% mutate(LR.sum = LR_SEP + LR_OCT + LR_NOV)
fall <- fall %>% mutate(NC.sum = NC_SEP + NC_OCT + NC_NOV)
fall <- fall %>% mutate(RL.sum = RL_SEP + RL_OCT + RL_NOV)
fall <- fall %>% mutate(all.sum = MR.sum + O.sum + LC.sum + LR.sum + NC.sum + RL.sum)

#sum for all parks
all.data <- all.data %>% mutate(all.sum = fall$all.sum + winter$all.sum + spring$all.sum + summer$all.sum)
summer <- summer %>% mutate(all.seasons.sum = all.data$all.sum)
winter <- winter %>% mutate(all.seasons.sum = all.data$all.sum)
spring <- spring %>% mutate(all.seasons.sum = all.data$all.sum)
fall <- fall %>% mutate(all.seasons.sum = all.data$all.sum)

