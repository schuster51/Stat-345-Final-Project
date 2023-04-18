library(rvest)
library(tidyverse)
library(rgenius)

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

###################

#Scrub for lyrics
#Idea
  #search https://genius.com/artist-name-song-name-lyrics
  #If found, paste lyrics into table
  #Else, Paste NA into table
  #Fill in the rest manually
  #Need EVERY WORD from EVERY SONG put into a list

#i only goes to 3 for stesting purposes. Put it at any value you want.
for(i in 1:3){
  #Grab and format song names
  songnameTemp = topSongs[i,3]
  songnameTemp2 = gsub("[[:punct:]]", "", songnameTemp)
  songnameFormat = gsub(" ", "-", songnameTemp2)
    #Some songs are split into 2; need an if statement.
    if(grepl("--", songnameFormat)){
      songnameFormat <- strsplit(songnameFormat, "--")[[1]]
      print(songnameFormat)
    } else{
      print(songnameFormat)
    }
  
  #Grab and format artist name
  artistnameTemp = topSongs[i,4]
  artistnameTemp2 = gsub("[[:punct:]]", "", artistnameTemp)
  artistnameTemp3 = gsub(" ", "-", artistnameTemp2)
  artistnameFormat = paste(artistnameTemp3, "-", sep = "")
  print(artistnameFormat)
  
  #Paste them together for a link
  geniusURLTemp = "https://genius.com/"
  geniusURL = paste(geniusURLTemp, artistnameFormat, songnameFormat, "-lyrics", sep = "")
  print(geniusURL)
}






