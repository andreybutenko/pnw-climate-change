library("stringr")
library(jsonlite)
library(dplyr)
library(httr)

#Get the count of words
getWordCount <- function(name) {
  return(str_count(name, "\\S+"))
}
#Get the number of words
getWords <- function(num, names) {
  noDoubleSpace <- gsub(' {2,}',' ', names)
  return(c(strsplit(noDoubleSpace, ' ')[[1]])[0:num])
}
filterWord <- function(word) {
  noSymbols = str_replace_all(word, "[[:punct:]]", "")
  if (!is.na(noSymbols) && nchar(noSymbols) > 0 && noSymbols == toupper(noSymbols)) {
    return("")
  }
  return(noSymbols)
}
#We dont want any symbols or abbreviations
filterWords <- function(words) {
  return(c(lapply(words, filterWord)))
}
#Get Search term using n words 
searchTerm <- function(word, num) {
  
  selectedWords <- getWords(num, word)
  
  filteredWords <- filterWords(selectedWords)
  
  return(paste(filteredWords, collapse = ' '))
}
queryDB <- function(limit, queryTerm) {
  base.url <- "https://developer.nps.gov/api/v1/parks"
  query.params <- list(limit=limit, start = 0, q = queryTerm, api_key = "TPvw2m9s0WEnljGSH6bAC0ZO4xq9YhlLq7T7g2Kj" )
  response <- GET(base.url, query = query.params)
  body <- content(response, "text")
  jsonD <- fromJSON(body)
  if (jsonD$total == 0) {
    return (list(first=NA, count=0))
  } else {
    data <- jsonD$data
    return (list(first=data[1,], count=nrow(data)))
  }
}
recursiveLogic <- function(wordsInTerm, hasGoneBack, term) {
  if (hasGoneBack) {
    #This is our last chance
    return(queryDB(wordsInTerm, searchTerm(term, wordsInTerm))$first)
  } else {
    lookFor <- searchTerm(term, wordsInTerm)
    query = queryDB(10, lookFor)
    if (query$count == 0) {
      return(recursiveLogic(wordsInTerm - 1, TRUE, term))
    } else if (query$count == 1) {
      return(query$first)
    } else {
      return(recursiveLogic(wordsInTerm + 1, FALSE, term))
    }
  }
}
#recursiveLogic(2, 1, false)
addState <- function(name) {
  print(name)
  query = recursiveLogic(2, FALSE, name)
  
  if (is.na(query)) {
    list(APIName="NA", State="NA")
  } else {
    return(list(APIName=query$fullName, State=query$states))
  }
}
setwd('/Users/Caleb/Coding/Informatics/pnw-climate-change')
current.data <- read.csv("data/NPSSpending.csv", stringsAsFactors = FALSE)
queries <- c(current.data$Park.Unit)
result <- lapply(queries, addState)
doThe <- do.call(rbind,lapply(result,data.frame))
additional.data <- mutate(current.data, state = doThe$State) %>%
                   mutate(name = doThe$APIName) %>%
                   filter(grepl("WA", state))
write.csv(additional.data, file.path("data", "ParksSpendingInWa.csv"), row.names=FALSE)


