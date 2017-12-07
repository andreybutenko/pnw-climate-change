library("stringr")
library(jsonlite)
library(dplyr)
library(httr)
library(ggplot2)



#Import and do some basic cleaning on the datasets
visitor.data <- read.csv("./data/OlympicNP/Visitors.csv", stringsAsFactors = FALSE)
names(visitor.data)[1] <- 'Year' # fix for windows machines :/
temperature.data <- read.csv("./data/OlympicNP/Temperature.csv", stringsAsFactors = FALSE)
names(temperature.data)[1] <- 'DATE'
dates <- strptime(paste0(as.character(temperature.data$DATE), "01"),format="%Y-%m%d")
temperature.data$Year <- format(dates, "%Y")
temperature.data$Month <- format(dates, "%m")
temperature.data$Date <- as.Date(as.POSIXct(dates))
air.data <- read.csv("./data/OlympicNP/AirQuality.csv", stringsAsFactors = FALSE)

#Utility Functions and variables
averageForMonth <- function(month) {
  return(mean(visitor.data[[month]], na.rm = TRUE))
}
months <-c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
months.undercase <-c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month.list <- list(Jan="January", Feb="February", Mar="March", Apr="April", May="May", Jun="June", Jul="July", Aug="August", Sep="September", Oct="October", Nov="November", Dec="December")
month.num <- list("01"="January", "02"="February", "03"="March", "04"="April", "05"="May", "06"="June", "07"="July", "08"="August", "09"="September", "10"="October", "11"="November", "12"="Dececember")
#PUBLIC FUNCTION to generate graphs on visitor data
visitorFilterGraph <- function(toggle, data) {
  if (toggle == TRUE) {
    if (data == "ANY") {
      return(visitorsPerMonth())
    } else {
      return(visitorsPerMonthOnYear(data))
    }
  } else {
    if (data == "ANY") {
      return(vistorsOverTheYears())
    } else {
      return(visitorsMonthsOverTheYears(data))
    }
  }
}

#PUBLIC FUNCTION to generate graphs on tempearature
temperatureGraph <- function(maxmin, arg, arguments) {
  edited.temp.data <- temperature.data
  table.title = "Temperature Over Time"
  if (arg == 1) {
    edited.temp.data <- edited.temp.data %>% 
      group_by(Year) %>%
      summarize(Average = mean(Average), 
                Max=mean(Max), 
                Min=mean(Min)) %>%
      mutate(Date=as.Date(paste0(Year,"-01-01")))
    
  } else if (arg == 2) {
    edited.temp.data <- edited.temp.data %>%
      filter(Month==arguments)
    table.title <- paste0(table.title, " During ", month.num[arguments])
  }
  
  g <- ggplot(edited.temp.data, aes(Date, Average, color="c2")) +
    geom_line() + geom_smooth(method='lm',formula=y~x, se = FALSE, color="darkgreen")
  
  
  if (maxmin) {
    g <- g + geom_line(aes(y=Max, color="c1")) +
      geom_line(aes(y=Min, color="c3"))
  }
  g <- g + xlab("Date") +
    ylab("Temperature (C)") + 
    scale_color_manual(name = "Lines",
                       breaks=c( "c1", "c2", "c3"),
                       values=c("c1"="red", "c2"="green", "c3"="blue"),
                       labels=c("Max", "Average" , "Min")) +
    ggtitle(table.title) + 
    theme(plot.title = element_text(hjust = 0.5))
  g
}

#PUBLIC FUNCTION air quality
airQuality <- function(year) {
  edited.air <- air.data
  text.x <- 0
  
  table.title <- ""
  if (year == "Any") {
    edited.air <- edited.air %>% 
      group_by(Year) %>%
      summarize(Average = mean(Average), Max=max(Max), Min=min(Min)) %>%
      mutate(Factor = Year)
    
      text.x <- ((max(edited.air$Year) - min(edited.air$Year)) / 2) + min(edited.air$Year)
      table.title <- "Air Quality Index From 1996-2016"
  } else {
    edited.air <- edited.air %>%
      filter(Year==year) %>%
      mutate(Factor = Month)
    
    text.x <- 6.5
    edited.air$Factor <- factor(edited.air$Factor, levels=edited.air$Month)
    table.title <- paste0("Air Quality Index During ", year)
  }
  
  height.max <- max(edited.air$Max)
  
  
  g <- ggplot(edited.air, aes(x=Factor, y=Average, ymax=Max, ymin=Min)) +
    geom_crossbar() + 
    xlab("Date") + 
    ylab("AQI Value (Lower is better)") +
    ggtitle(table.title) + 
    theme(plot.title = element_text(hjust = 0.5))
  
  g <- classificationAnnotations(g, text.x, height.max)
  
  if (year != "Any") {
    g <- g + scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                              labels=months) +
      xlab("Month")
  }
  
  g
}

classificationAnnotations <- function(g, text.x, height.max) {
  const.alpha <- 0.3
  if (height.max <= 50) {
    g <- g + annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=const.alpha, fill="green") +
      annotate(geom="text", x = text.x, y = 25, label="Good", alpha = 0.5, color = "darkgrey")
    
  } else if (height.max <= 100) {
    g <- g + annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=50, alpha=const.alpha, fill="green") +
      annotate("rect", xmin=-Inf, xmax=Inf, ymin=50, ymax=Inf, alpha=const.alpha, fill="yellow") +
      annotate(geom="text", x = text.x, y = 25, label="Good", alpha = 0.5, color = "darkgrey") + 
      annotate(geom="text", x = text.x, y = 75, label="Moderate", alpha = 0.5, color = "darkgrey")
  } else if (height.max <= 150) {
    g <- g + annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=50, alpha=const.alpha, fill="green") +
      annotate("rect", xmin=-Inf, xmax=Inf, ymin=50, ymax=100, alpha=const.alpha, fill="yellow") + 
      annotate("rect", xmin=-Inf, xmax=Inf, ymin=100, ymax=Inf, alpha=const.alpha, fill="orange") +
      annotate(geom="text", x = text.x, y = 25, label="Good", alpha = 0.5, color = "darkgrey") + 
      annotate(geom="text", x = text.x, y = 75, label="Moderate", alpha = 0.5, color = "darkgrey") + 
      annotate(geom="text", x = text.x, y = 125, label="Unhealthy For Sensative", alpha = 0.5, color = "darkgrey") 
  } else if (height.max <= 200) {
    g <- g + annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=50, alpha=const.alpha, fill="green") +
      annotate("rect", xmin=-Inf, xmax=Inf, ymin=50, ymax=100, alpha=const.alpha, fill="yellow") + 
      annotate("rect", xmin=-Inf, xmax=Inf, ymin=100, ymax=150, alpha=const.alpha, fill="orange") +
      annotate("rect", xmin=-Inf, xmax=Inf, ymin=150, ymax=Inf, alpha=const.alpha, fill="red") +
      annotate(geom="text", x = text.x, y = 25, label="Good", alpha = 0.5, color = "darkgrey") + 
      annotate(geom="text", x = text.x, y = 75, label="Moderate", alpha = 0.5, color = "darkgrey") + 
      annotate(geom="text", x = text.x, y = 125, label="Unhealthy For Sensative", alpha = 0.5, color = "darkgrey") + 
      annotate(geom="text", x = text.x, y = 175, label="Unhealthy", alpha = 0.5, color = "darkgrey")
  } else {
    g <- g + annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=50, alpha=const.alpha, fill="green") +
      annotate("rect", xmin=-Inf, xmax=Inf, ymin=50, ymax=100, alpha=const.alpha, fill="yellow") + 
      annotate("rect", xmin=-Inf, xmax=Inf, ymin=100, ymax=150, alpha=const.alpha, fill="orange") +
      annotate("rect", xmin=-Inf, xmax=Inf, ymin=150, ymax=200, alpha=const.alpha, fill="red") + 
      annotate("rect", xmin=-Inf, xmax=Inf, ymin=200, ymax=Inf, alpha=const.alpha, fill="black") +
      annotate(geom="text", x = text.x, y = 25, label="Good", alpha = 0.5, color = "darkgrey") + 
      annotate(geom="text", x = text.x, y = 75, label="Moderate", alpha = 0.5, color = "darkgrey") + 
      annotate(geom="text", x = text.x, y = 125, label="Unhealthy For Sensative", alpha = 0.5, color = "darkgrey") + 
      annotate(geom="text", x = text.x, y = 175, label="Unhealthy", alpha = 0.5, color = "darkgrey") + 
      annotate(geom="text", x = text.x, y = 225, label="Very Unhealthy", alpha = 0.5, color = "darkgrey")
    
  }
  return(g)
}



#X: Month, Y: Visitors (bar chart) filter by year ()
visitorsPerMonthOnYear <- function(year) {
  filtered.frame <- filter(visitor.data, Year == year) %>%
    select(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
  filtered.frame[is.na(filtered.frame)] <- 0
  data <- c(unlist(filtered.frame[1,])) / 10000
  
  frame <- data.frame(month = months, visitors = as.vector(data))
  frame$month <- factor(frame$month, levels = months)
  
  p <- ggplot(data=frame, aes(x=month, y=visitors)) + 
    geom_bar(stat="identity", fill="darkgreen") + 
    geom_text(aes(label=round(visitors)), vjust=1.6, color="yellow", size=3.5) +
    xlab("Month") +
    ylab("Thousands Of Visitors") + 
    ggtitle(paste0("Visitors Per Month In ", year)) +
  theme(plot.title = element_text(hjust = 0.5))
  p
}
#X: Month, Y: Visitors (bar chart) (no, no)
visitorsPerMonth <- function() {
  data <- c(sapply(months.undercase, averageForMonth)) / 1000
  frame <- data.frame(month = months, visitors = data)
  frame$month <- factor(frame$month, levels = months)
  
  p <- ggplot(data=frame, aes(x=month, y=visitors)) + 
    geom_bar(stat="identity", fill="darkgreen") + 
    geom_text(aes(label=round(visitors)), vjust=1.6, color="yellow", size=3.5) +
    xlab("Month") +
    ylab("Thousands Of Visitors") + 
    ggtitle("Average Visitors Per Month") +
    theme(plot.title = element_text(hjust = 0.5))
  p
}
#X: Years, Y: Visitors filter by month (Line graph) (no, yes)
visitorsMonthsOverTheYears <- function(month) {
  x <- visitor.data$Year 
  y <- visitor.data[[month]] / 1000
  
  frame = data.frame(year=x, visitors=y)
  
  p <- ggplot(data = frame, aes(year, visitors)) +
    geom_line(color="darkgreen") +
    geom_smooth(method='lm',formula=y~x, se = FALSE, color="green") +
    xlab("Year") +
    ylab("Thousands Of Visitors") + 
    ggtitle(paste0("Visitors From 1979-2016 During ", month.list[month])) + 
    theme(plot.title = element_text(hjust = 0.5))
  p
}
#X: Years, Y: Visitors (Line graph) (no, no)
vistorsOverTheYears <- function() {
  x <- visitor.data$Year 
  y <- visitor.data$Total.Number.of.Visitors.for.the.Year / 1000000
  frame = data.frame(year=x, visitors=y)
  
  p <- ggplot(data = frame, aes(year, visitors)) +
    geom_line(color="darkgreen") +
    geom_smooth(method='lm',formula=y~x, se = FALSE, color="green") +
    xlab("Year") +
    ylab("Millions Of Visitors") +
    ggtitle("Visitors From 1979-2016") + 
    theme(plot.title = element_text(hjust = 0.5))
  p
}
visitorFilterGraph(TRUE, "ANY")#
visitorFilterGraph(TRUE, "1999")#
visitorFilterGraph(FALSE, "ANY")#
visitorFilterGraph(FALSE, "Mar")#
temperatureGraph(FALSE, 0, "ANY") #
temperatureGraph(TRUE, 1, "ANY") #
temperatureGraph(TRUE, 2, "01") #
airQuality("2005")
airQuality("Any")