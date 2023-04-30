library(rvest)
library(tidyverse)
#library(rgenius)  #Not used
library(wordcloud2)

#Used Ben's code to make an initial table to bind all future years to
url1958 = "https://en.wikipedia.org/wiki/Billboard_year-end_top_50_singles_of_1958"
read1958 = read_html(url1958)
table_1958 = read1958 %>% html_table
table_1958 = data.frame(table_1958[[1]])
year = rep(1958, each = 51)
new_table = cbind(table_1958, year)
table_1958 = setNames(object = new_table, c("No.", "Title", "Artist", "Year")) %>%
  relocate("Year", .before = "No." )

  #Separates double songs
  if (any(grepl("/", table_1958$Title))) {
    table_1958 <- table_1958 %>%
      separate_rows(Title, sep = "/")
  }

topSongs = table_1958[1:26,]

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
  
    #Separates double songs
    if (any(grepl("/", tableTemp$Title))) {
      tableTemp <- tableTemp %>%
        separate_rows(Title, sep = "/")
    }
  
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
  
    #Separates double songs
    if (any(grepl("/", tableTemp$Title))) {
      tableTemp <- tableTemp %>%
        separate_rows(Title, sep = "/")
    }
  
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
  
    #Separates double songs
    if (any(grepl("/", tableTemp$Title))) {
      tableTemp <- tableTemp %>%
        separate_rows(Title, sep = "/")
    }
  
  topSongs = rbind(topSongs, tableTemp)
  print(yearVar)
}

###################

#Generate URLs

URLVector = c() #Can use this vector (once filled) in later functions.

for(i in 1:1300){
  #Grab and format song names
  songnameTemp = topSongs[i,3]
  songnameTemp2 = gsub("[[:punct:]]", "", songnameTemp)
  songnameFormat = gsub(" ", "-", songnameTemp2) %>%
    tolower()
  
    #Some songs are split into 2; need an if statement.
    if(grepl("--", songnameFormat)){
      songnameFormat = strsplit(songnameFormat, "--")[[1]] %>%
        tolower()
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
  
    #Need to remove featured artists. If there is a featured artist, it will drop their name. Otherwise, it will print song as-is (or multiple if applicable)
    if(grepl("-featuring", artistnameFormat)){
      artistnameFormat = strsplit(artistnameFormat, "featuring")[[1]]
      print(songnameFormat[1])
      geniusURLTemp = "https://genius.com/"
      geniusURL = paste(geniusURLTemp, artistnameFormat, songnameFormat, "-lyrics", sep = "")
      print(geniusURL[1])
      URLVector = append(URLVector, geniusURL[1])
    } else{
      geniusURLTemp = "https://genius.com/"
      geniusURL = paste(geniusURLTemp, artistnameFormat, songnameFormat, "-lyrics", sep = "")
      print(geniusURL)
      URLVector = append(URLVector, geniusURL)
    }
}

###################

#Grab and format lyrics from links

#Lyric function from ben
get_lyrics <- function(urls) {
  lyrics_vec <- vector(mode = "character", length(urls))
  
  for (i in seq_along(urls)) {
    tryCatch({
      html <- read_html(urls[i])
      lyrics_node <- html_node(html, ".Dzxov , .jAzSMw , i") #.Dzxov
      lyrics_text <- html_text(lyrics_node)
      
        #Formatting the lyrics correctly. These can PROBABLY be written into one line, but it sounds tedious so I didn't bother.
        lyricsFormatTemp =  gsub("\\[.*?\\]", "", lyrics_text) #Removes square brackets
        lyricsFormatTemp2 = gsub("[']", "", lyricsFormatTemp) #Removes apostrophes
        lyricsFormat = gsub("(?<=[a-z])(?=[A-Z])", " ", lyricsFormatTemp2, perl=TRUE) #Adds spaces before capitals

      lyrics_vec[i] <- lyricsFormat
      print(URLVector[i])
    }, error = function(e) {
      lyrics_vec[i] <- "Not Found"
    })
  }
  return(lyrics_vec)
}

#Can grab all links by indexing URLVector[]
#DISCLAIMER: Will take ~1 hour to run full URLVector
testlyric = get_lyrics(URLVector[1:10])


#Put ben's code into a function to test various values.
wordcount = function(x){
  words = strsplit(tolower(x), "\\W+")
  word_counts = table(words)
  word_counts[order(-word_counts)]
}

####################

#Testing

#test: Starboy
starboy = get_lyrics(URLVector[3854])
sbtab = as.data.frame(wordcount(starboy))
#Appears it isn't grabbing all verses of song??


songsFull = read_csv("Test_file.csv")
wordcount(songsFull)


###################

#Formatting CSV
testFile = read_csv("Test_file.csv")

#This is the final songs + lyrics dataframe
songsWithLyrics = cbind(topSongs, testFile) %>%
  setNames(c("Year", "No.", "Title", "Artist", "Lyrics"))



