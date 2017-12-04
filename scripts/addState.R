library("stringr")
library(jsonlite)
library(dplyr)
library(httr)

setwd('/Users/Caleb/Coding/Informatics/pnw-climate-change')
source('scripts/api_keys.R')

#Get the count of words
getWordCount <- function(name) {
  return(str_count(name, "\\S+"))
}
#Get the number of words
getWords <- function(num, names) {
  no.double.space <- gsub(' {2,}',' ', names)
  return(c(strsplit(no.double.space, ' ')[[1]])[0:num])
}
filterWord <- function(word) {
  no.symbols = str_replace_all(word, "[[:punct:]]", "")
  if (!is.na(no.symbols) && nchar(no.symbols) > 0 && no.symbols == toupper(no.symbols)) {
    return("")
  }
  return(no.symbols)
}
#We dont want any symbols or abbreviations
filterWords <- function(words) {
  return(c(lapply(words, filterWord)))
}
#Get Search term using n words 
searchTerm <- function(word, num) {
  
  selected.words <- getWords(num, word)
  
  filtered.words <- filterWords(selected.words)
  
  return(paste(filtered.words, collapse = ' '))
}
queryDB <- function(limit, query.term) {
  base.url <- "https://developer.nps.gov/api/v1/parks"
  query.params <- list(limit=limit, start = 0, q = query.term, api_key = nps.api )
  response <- GET(base.url, query = query.params)
  body <- content(response, "text")
  jason.d <- fromJSON(body)
  if (jason.d$total == 0) {
    return (list(first=NA, count=0))
  } else {
    data <- jason.d$data
    return (list(first=data[1,], count=nrow(data)))
  }
}
recursiveLogic <- function(words.in.term, has.gone.back, term) {
  if (has.gone.back) {
    #This is our last chance
    return(queryDB(words.in.term, searchTerm(term, words.in.term))$first)
  } else {
    lookFor <- searchTerm(term, words.in.term)
    query = queryDB(10, lookFor)
    if (query$count == 0) {
      return(recursiveLogic(words.in.term - 1, TRUE, term))
    } else if (query$count == 1) {
      return(query$first)
    } else {
      return(recursiveLogic(words.in.term + 1, FALSE, term))
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


current.data <- read.csv("data/NPSSpending.csv", stringsAsFactors = FALSE)
queries <- c(current.data$Park.Unit)
result <- lapply(queries, addState)
as.table <- do.call(rbind,lapply(result,data.frame))
additional.data <- mutate(current.data, state = as.table$State) %>%
                   mutate(name = as.table$APIName) %>%
                   filter(grepl("WA", state))
write.csv(additional.data, file.path("data", "ParksSpendingInWa.csv"), row.names=FALSE)


