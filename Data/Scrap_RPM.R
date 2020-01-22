library(tidyverse)
library(httr)
library(stringdist)
library(rvest)    
library(stringr)   

# Real plus / minus https://www.espn.com/nba/story/_/id/10740818/introducing-real-plus-minus

# scrap the RPM data ---------------------------------------------------

# set the base url
base.url <- "http://www.espn.com/nba/statistics/rpm/_/year/2017"
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
    matrix(ncol = 9, byrow = TRUE) %>% 
    as_tibble()
  evens <- evens %>% 
    html_children() %>% 
    html_text() %>% 
    matrix(ncol = 9, byrow = TRUE) %>% 
    as_tibble()
  
  # combine those two tables into one and store in the list
  tables[[i]] <- bind_rows(odds, evens)
}

# create final dataframe of salaries by combining each pages' results
RPM.df <- tables %>% 
  bind_rows() %>% 
  setNames(c("Rank", "Player", "Team", "GP", "MPG", "ORPM", "DRPM", "RPM", "WINS")) %>% 
  mutate_at(c("Rank", "GP", "MPG", "ORPM", "RPM", "WINS"), as.numeric) %>% 
  separate(col = Player, into = c("Player", "Pos"),
           sep = ", ") %>% 
  arrange(Rank)

# inspect the data
head(RPM.df)

# check for duplicates
table(duplicated(RPM.df))
dim(RPM.df)[1] == length(unique(RPM.df$Player))



# Match the names to the stats dataframe ----------------------------------

# core problem is that we need to join the salary data with the stats data
#   but the names do not match so we need to fuzzy match

# read in the stats data
stats.df <- read_csv('Data/season_stats_clean.csv')

# how many players are an exact match in the salary data? 
table(stats.df$Player %in% salary.df$Player)

# which players are not in the salary data
stats.df[!(stats.df$Player %in% salary.df$Player),] %>% View()

# other data inspections
unique(stats.df$Tm)
length(unique(stats.df$Player))
length(unique(salary.df$Player))
table(salary.df$Player %in% stats.df$Player)
unique(stats.df$Pos) %in% unique(salary.df$Pos)
unique(salary.df$Pos) %in% unique(stats.df$Pos) 
length(unique(salary.df$Team))
length(unique(stats.df$Tm))

get_top_matches <- function(current.name, names.to.match, n = 5){
  # function returns that top n matches of the old.name
  #   within the new.names list via fuzzy string matching
  
  scores <- stringsim(current.name, names.to.match, method = "osa")
  names.to.match[rev(order(scores))][1:n]
}

# test the function
get_top_matches(stats.df$Player[1], salary.df$Player)

# apply the function across the entire list to generate a data.frame
#  containing the current.name and it's top 5 best matches
all.matches <- lapply(stats.df$Player, get_top_matches, names.to.match = salary.df$Player) %>% 
  unlist() %>% 
  matrix(ncol = 5, byrow = TRUE) %>% 
  as_tibble() %>% 
  bind_cols(Current.name = stats.df$Player, .) %>%
  setNames(c("Current.name", paste0("Match.", 1:5))) %>% 
  mutate(Index = row_number())

# return just the names that don't match along with their best matches 
unmatched <- all.matches[all.matches$Current.name != all.matches$Match.1,]

# visually inspect then manually adjust the names so they do match
View(unmatched)

# this are the indices of the ones to adjust
# 1:7 2
# 8 3
# 9:12 2
# 13:16 2
# 17 # unsure, who is Gary Neal? See manual fix below
# 18:27 2

# create a new column Matched.name and fill it with the corrected name
unmatched$Matched.name <- NA
unmatched[c(1:7, 9:12, 13:16, 18:27), "Matched.name"] <- unmatched[c(1:7, 9:12, 13:16, 18:27), "Match.1"]
unmatched[8, "Matched.name"] <- unmatched[8, "Match.2"] 

# create a new column New.name containing all the matched names
all.matches$New.name <- all.matches$Current.name
all.matches$New.name[all.matches$Index %in% unmatched$Index] <- unmatched$Matched.name

# add the salary to the original stats.df dataframe
stats.df$New.name <- all.matches$New.name
stats.df$Salary <-  salary.df$Salary[match(stats.df$New.name, salary.df$Player)]
stats.df <- select(stats.df, -New.name)

# Add in Gary Neal's salary manually https://hoopshype.com/player/gary-neal/salary/
stats.df$Salary[stats.df$Player == "Gary Neal"] <- 72193

# visually check if the top salaries seem correct
stats.df %>% 
  select(Player, Salary) %>% 
  arrange(desc(Salary))

# write the final data.frame to csv
write.csv(stats.df, 'Data/season_stats_clean.csv', row.names = FALSE)



