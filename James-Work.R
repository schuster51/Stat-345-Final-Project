library(rvest)
library(tidyverse)

#Used Ben's code to make an initial table to bind all future years to
url1958 = "https://en.wikipedia.org/wiki/Billboard_year-end_top_50_singles_of_1958"
read1958 = read_html(url1958)
table_1958 = read1958 %>% html_table
table_1958 = data.frame(table_1958[[1]])
year = rep(1958, each = 51)
new_table = cbind(table_1958, year)
table_1958 = setNames(object = new_table, c("No.", "Title", "Artist", "Year")) %>%
  relocate("Year", .before = "No." )
topSongs = table_1958[1:20,]



for(yearVar in 1959:2011){
  #Using the iterated yearVar variable to get the url for any given year
  urlTemp = "https://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_"
  yearString = as.character(yearVar)
  url = paste(urlTemp, yearString, sep = "")
  readUrl = read_html(url)
  
  tableTemp = readUrl %>% html_table 
  tableTemp = data.frame(tableTemp[[1]])
  
  #Need an if statement, because the song number is either referenced as X. or No. depending on the year
  if("X." %in% colnames(tableTemp)){
    tableTemp = tableTemp %>% filter(as.numeric(X.) <= 20)
  }else{
    tableTemp = tableTemp %>% filter(as.numeric(No.) <= 20)
  }
  yearCol = rep(yearVar, each = 20)
  tableTemp.2 = cbind(tableTemp, yearCol[1:10]) 
  tableTemp = setNames(object = tableTemp.2, c("No.", "Title", "Artist", "Year")) %>%
    relocate("Year", .before = "No." )
  topSongs = rbind(topSongs, tableTemp)
  print(yearVar)
}

#2012 and 2013 use a different table for some reason, so they must be done individually.

for(yearVar in 2012:2013){
  #Using the iterated yearVar variable to get the url for any given year
  urlTemp = "https://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_"
  yearString = as.character(yearVar)
  url = paste(urlTemp, yearString, sep = "")
  readUrl = read_html(url)
  
  tableTemp = readUrl %>% html_table 
  tableTemp = data.frame(tableTemp[[2]])
  tableTemp = tableTemp %>% filter(as.numeric(No.) <= 20)
  yearCol = rep(yearVar, each = 20)
  tableTemp.2 = cbind(tableTemp, yearCol[1:10]) 
  tableTemp = setNames(object = tableTemp.2, c("No.", "Title", "Artist", "Year")) %>%
    relocate("Year", .before = "No." )
  topSongs = rbind(topSongs, tableTemp)
  print(yearVar)
}

#Using the original code to get the rest of the years.

for(yearVar in 2014:2022){
  #Using the iterated yearVar variable to get the url for any given year
  urlTemp = "https://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_"
  yearString = as.character(yearVar)
  url = paste(urlTemp, yearString, sep = "")
  readUrl = read_html(url)
  
  tableTemp = readUrl %>% html_table 
  tableTemp = data.frame(tableTemp[[1]])
  
  #Need an if statement, because the song number is either referenced as X. or No. depending on the year
  if("X." %in% colnames(tableTemp)){
    tableTemp = tableTemp %>% filter(as.numeric(X.) <= 20)
  }else{
    tableTemp = tableTemp %>% filter(as.numeric(No.) <= 20)
  }
  yearCol = rep(yearVar, each = 20)
  tableTemp.2 = cbind(tableTemp, yearCol[1:10]) 
  tableTemp = setNames(object = tableTemp.2, c("No.", "Title", "Artist", "Year")) %>%
    relocate("Year", .before = "No." )
  topSongs = rbind(topSongs, tableTemp)
  print(yearVar)
}
