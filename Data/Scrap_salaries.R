library(tidyverse)
library(httr)
library(hms)
library(lubridate)
library(DescTools)
library(stringdist)
library(rvest)    
library(stringr)   

# scrap the salary data ---------------------------------------------------

# set the base url
base.url <- "http://www.espn.com/nba/salaries/_/year/2017"
response <- read_html(base.url)

# find the number of pages to loop through
page.count <- html_nodes(x = response,
                         xpath = '//div[contains(@class, "page-numbers")]') %>% 
  html_text() %>% 
  str_remove("1 of ") %>% 
  as.numeric()

# loop through each page and scrap the table
tables <- list()
for (i in 1:page.count){
  
  # set the url
  url <- paste0(base.url, "/page/", i)
  response <- read_html(url)
  
  # scrape the odd rows then the even rows from the webpage's table
  # performing it rowwise because the columns do not have class names that
  #   we can reference
  odds <- html_nodes(x = response,
                     xpath = '//tr[contains(@class, "oddrow")]')
  evens <- html_nodes(x = response,
                      xpath = '//tr[contains(@class, "evenrow")]')
  
  # for each odds and evens, create a data.frame from the html
  odds <- odds %>% 
    html_children() %>% 
    html_text() %>% 
    matrix(ncol = 4, byrow = TRUE) %>% 
    as_tibble()
  evens <- evens %>% 
    html_children() %>% 
    html_text() %>% 
    matrix(ncol = 4, byrow = TRUE) %>% 
    as_tibble()
  
  # combine those two tables into one and store in the list
  tables[[i]] <- bind_rows(odds, evens)
}

# create final dataframe of salaries by combining each pages' results
salary.df <- tables %>% 
  bind_rows() %>% 
  setNames(c("Rank", "Player", "Team", "Salary")) %>% 
  mutate(Rank = as.numeric(Rank),
         Salary = substr(Salary,
                         start = 2,
                         stop = length(Salary)) %>% 
           str_remove_all(., ",") %>% 
           as.numeric()) %>% 
  separate(col = Player, into = c("Player", "Pos"),
           sep = ", ") %>% 
  arrange(Rank)

# inspect the data
head(salary.df)

# check for duplicates
table(duplicated(salary.df))
dim(salary.df)[1] == length(unique(salary.df$Player))

# Match the names to the stats dataframe ----------------------------------

# core problem is that we need to join the salary data with the stats data
#   but the names do not match so we need to fuzzy match

# read in the stats data
stats.df <- read_csv('Data/season_stats_clean.csv')

# remove extraneous columns
stats.df <- stats.df[, c("Player", "Tm", "Pos")]
names(stats.df)[2] <- "Team"

unique(stats.df$Team)
table(salary.df$Player %in% stats.df$Player)
unique(stats.df$Pos) %in% unique(salary.df$Pos)
unique(salary.df$Pos) %in% unique(stats.df$Pos) 

length(unique(salary.df$Team))
length(unique(stats.df$Team))



# subway method -----------------------------------------------------------


# add lat long ------------------------------------------------------------

# core problem is that the turnstile data doesn't have lat long info
# goal is to merge with a dataset known list of stations with lat long
# but dataset station names don't match so we need to fuzzy match

#import list of stations and tidy | need this for lat long | save for later use
# stations.latlong.df <- GET("https://data.cityofnewyork.us/api/views/kk4q-3rt2/rows.csv?accessType=DOWNLOAD")
# stations.latlong.df <- content(stations.latlong.df)
# write_csv(stations.latlong.df, "Subway-turnstiles/Data/stations.latlong.df.csv")
stations.latlong.df <- read_csv("Subway-turnstiles/Data/stations.latlong.df.csv")

# change names to proper case
names(stations.latlong.df) <- sapply(names(stations.latlong.df), toproper)

#seperate out the lat/long
stations.latlong.df <- stations.latlong.df %>%
  mutate(The_geom = str_remove(The_geom, "POINT [(]") %>% str_remove(., "[)]")) %>%
  separate(The_geom, into = c("Long", "Lat"), sep = " ") %>%
  mutate(Lat = as.double(Lat),
         Long = as.double(Long)) %>%
  rename(Station = Name) %>%
  select(Station, Lat, Long, Line)

#check the station names; most station names don't match so need to fuzzy match
# unique(turnstile.df$Station)
# unique(stations.latlong.df$Station)

# need to match on station name and line; check line first then match name
# match subway line letters by splitting apart and checking individually
# need to clean up stations.latlong.df$Line first

# remove "express" so E doesn't match it
stations.latlong.df$Line <- str_remove_all(stations.latlong.df$Line, "Express")

# split out the letters from the lines column
stations.latlong.df$Line <- str_split(stations.latlong.df$Line, "-")
turnstile.df$Linename <- str_split(turnstile.df$Linename, "")

# make station names lower case so they match better
stations.latlong.df$Station <- tolower(stations.latlong.df$Station)
turnstile.df$Station <- tolower(turnstile.df$Station)

# replace ave and avenue with av
stations.latlong.df$Station <- str_replace_all(stations.latlong.df$Station, " ave"," av")
stations.latlong.df$Station <- str_replace_all(stations.latlong.df$Station, "ave ","av ")
turnstile.df$Station <- str_replace_all(turnstile.df$Station, "avenue ","av ")
turnstile.df$Station <- str_replace_all(turnstile.df$Station, " ave"," av")

# replace street with st
turnstile.df$Station <- str_replace_all(turnstile.df$Station, " street"," st")

# replace road with rd
stations.latlong.df$Station <- str_replace_all(stations.latlong.df$Station, " road"," rd")
turnstile.df$Station <- str_replace_all(turnstile.df$Station, " road"," rd")

# replace place with pl
turnstile.df$Station <- str_replace_all(turnstile.df$Station, " place"," pl")

# remove "ths" and "rd" after street numbers
stations.latlong.df$Station[grep("[0-9]th", stations.latlong.df$Station)] <- str_remove_all(stations.latlong.df$Station[grep("[0-9]th", stations.latlong.df$Station)], "th")
stations.latlong.df$Station[grep("[0-9]rd", stations.latlong.df$Station)] <- str_remove_all(stations.latlong.df$Station[grep("[0-9]rd", stations.latlong.df$Station)], "rd")
turnstile.df$Station[grep("[0-9]th", turnstile.df$Station)] <- str_remove_all(turnstile.df$Station[grep("[0-9]th", turnstile.df$Station)], "th")

# remove "nd"s 
stations.latlong.df$Station[grep("[0-9]nd", stations.latlong.df$Station)] <- str_remove_all(stations.latlong.df$Station[grep("[0-9]nd", stations.latlong.df$Station)], "nd")


get_new_name <- function(old.name, subway.lines){
  
  # function returns a matching station name (plus line, lat, long) from the new lat/long dataset
  # it first creates a possible list of new stations based on which subway
  #   lines are matched
  # then it fuzzy matches the name of the old.name against the list
  #   of new station names
  
  # determine the probability a station matches by comparing the number of lines 
  # that match (the intersect) to the total amount (the union) of lines
  match.probs <- sapply(stations.latlong.df$Line, function(new.line) {
    n.intst <- length(intersect(subway.lines, new.line))
    n.union <- length(union(subway.lines, new.line))
    prob <- n.intst / n.union
    return(prob)
  })
  
  # find the station names that match the best: i.e. include matches that 
  #  are at least 50% as good as the top match
  match.stations <- stations.latlong.df[match.probs >= max(match.probs) * 0.5,]
  
  # now fuzzy match the station name within this list
  new.name <- stringsim(old.name, unlist(match.stations$Station), method = "osa") %>% which.max(.) %>% match.stations[.,]
  
  return(new.name)
}

# get only unique pairs of station and lines then get rid of NA line
station.line.pairs <- turnstile.df[,c("Old.station", "Old.linename", "Station", "Linename")][!duplicated(turnstile.df[,c("Old.station", "Old.linename", "Station", "Linename")]),]
station.line.pairs <- station.line.pairs[!is.na(station.line.pairs$Station),]

# apply the get_new_name function over the unique pairs data frame
station.line.pairs <- lapply(1:nrow(station.line.pairs), function(index) {
  get_new_name(station.line.pairs$Station[[index]], station.line.pairs$Linename[[index]])
}) %>%
  bind_rows() %>%
  bind_cols(station.line.pairs, .) %>%
  rename(New.name = Station1,
         New.line = Line)

#manually fix some of the big stations
station.line.pairs[6, 5:8] <- stations.latlong.df[145,] #herald sq
station.line.pairs[9, 5:8] <- stations.latlong.df[105,] #union sq - 14 st
station.line.pairs[24, 5:8] <- stations.latlong.df[281,] #botanic
station.line.pairs[44, 5:8] <- stations.latlong.df[36,] #union st
station.line.pairs[64, 5:8] <- stations.latlong.df[336,] # 86 st [64]
station.line.pairs[132, 5:8] <- stations.latlong.df[396,] # 181 st [132]
station.line.pairs[156, 5:8] <- stations.latlong.df[410,] # canal st [156]
station.line.pairs[158, 5:8] <- stations.latlong.df[409,] # world trade ct [158]
# station.line.pairs[249, 5:8] <- stations.latlong.df[,] # 2 av [249] no second ave in the dataset
station.line.pairs[288, 5:8] <- stations.latlong.df[195,] # christopher [288]
# station.line.pairs[289, 5:8] <- stations.latlong.df[195,] # 9 st [289] no ninth st in the dataset
station.line.pairs[290, 5:8] <- stations.latlong.df[439,] # 14 st [290]
station.line.pairs[291, 5:8] <- stations.latlong.df[96,] # twenty third [291]
# station.line.pairs[292, 5:8] <- stations.latlong.df[96,]# thirty st [292] no match in the dataset
station.line.pairs[330, 5:8] <- stations.latlong.df[178,] # 181 st [330]
station.line.pairs[336, 5:8] <- stations.latlong.df[267,] # 231 st [336]
station.line.pairs[342, 5:8] <- stations.latlong.df[467,] # spring [342]
station.line.pairs[344, 5:8] <- stations.latlong.df[1,] # astor pl [344]
station.line.pairs[345, 5:8] <- stations.latlong.df[105,] # 14 st union sq [345]
station.line.pairs[346, 5:8] <- stations.latlong.df[92,] # 23rd [346]
station.line.pairs[347, 5:8] <- stations.latlong.df[200,] # 28 st [347]
station.line.pairs[348, 5:8] <- stations.latlong.df[32,] # 33 st [348]
station.line.pairs[350, 5:8] <- stations.latlong.df[85,] # 51 st [350]
station.line.pairs[352, 5:8] <- stations.latlong.df[102,] # 68st hunter [352]
station.line.pairs[355, 5:8] <- stations.latlong.df[33,] # 96st [355]
station.line.pairs[356, 5:8] <- stations.latlong.df[458,] # 103 [356]
station.line.pairs[357, 5:8] <- stations.latlong.df[450,] # 110 [357]
station.line.pairs[358, 5:8] <- stations.latlong.df[462,] # 116 [358]

# process to manually look up matches
# indices <- grep("116", stations.latlong.df$Station)
# View(stations.latlong.df %>% rowid_to_column() %>% .[indices,], title = "matches")
# View(station.line.pairs[358,], title = "actual")

# histogram of string sim scores of the station name matches
station.line.pairs %>%
  rowwise() %>%
  mutate(Score = stringsim(Station, New.name)) %>%
  ggplot(aes(x = Score)) +
  geom_histogram(color = "white", binwidth = 0.05) +
  labs(title = "Histogram of the string match scores comparing the final station names",
       subtitle = "Higher is better; 1 indicates perfect match")
