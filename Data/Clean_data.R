library(data.table)
library(dplyr)
library(knitr)
library(tibble)


# Description -------------------------------------------------------------
# The script cleans the input data from the Seasons_Stats.csv and the 
# nba_team....csv files. It removes the duplicates created by players switching
# teams mid-season. It combines the two files and then writes to a new csv
# in the "Data" folder. 

# define file name for analysis
filename <- 'Inputs/Seasons_Stats.csv'
filename_team <- 'Inputs/nba_team_salaries_win_pct_2016_2017.csv'

# Data cleaning -----------------------------------------------------------

# load and preview data
nba_raw <- read.csv(filename)
head(nba_raw)

# Some column names in the data contain invalid characters when reading the file, we will remove these.
# remove pre-pended characters from column names
char <- 'X'
colnames(nba_raw) <- gsub(char, '', colnames(nba_raw))

### Subset fields and include only the most recent year

# subset to most recent year and fields for analysis
year <- nba_raw$Year[which.max(nba_raw$Year)]
fields <- c('Year', 'Player', 'Tm', 'Pos', 'Age', 'G', 
            'MP','FG', 'FGA', '3P', '3PA', '2P', '2PA',
            'FT', 'FTA', 'TRB', 'AST', 'STL', 'BLK', 'PTS',
            'VORP', 'PER'
)

# fields needed
nba <- subset(nba_raw, Year == year, select = fields)

# fetch fields to convert to per game metrics
nba_feats_totals <- c('MP', 'FG', 'FGA', '3P', '3PA', '2P',
                      '2PA', 'FT', 'FTA', 'TRB', 'AST', 'STL',
                      'BLK', 'PTS')

# per game column names ('pg')
nba_feats_totals_new_names <- paste0(nba_feats_totals, '_pg')
nba_per_game <- data.frame(sapply(X = nba[ , nba_feats_totals],
                                  FUN = function(x) {x / nba$G}))

# rename columns
setnames(nba_per_game, 
         old = names(nba_per_game), 
         new = nba_feats_totals_new_names)

# join to nba data
nba <- cbind(nba, nba_per_game)

### clean up rows for players w/ multiple teams

# fetch number of teams a player has been on
num_teams <- data.frame(nba 
                        %>% count(Player) 
                        %>% arrange(desc(n))
                        %>% rename('num_teams' = 'n'))
# check data
head(num_teams)

# Inspect one player on multiple teams
subset(nba, Player == 'Ersan Ilyasova')

# There is a record for each team a player was on. 
# For example, Ersan Ilysaova played on 3 teams in 2017, 
# so there are three rows. To work around this, we will just 
# assign the team the player has played the most minutes on 
# to their total ('TOT') row. For Ilysasova, this would be PHI 
# since he played the most minutes there vs. his other teams.

### Assign team based on max minutes a player has played for a team
# Add columns for number of teams a player has been on to dataset
nba <- left_join(nba, num_teams, by = 'Player')

# filter players who were on more than one team
players_mult_teams <- nba %>% filter(num_teams > 1)

# group by player and get max minutes each player played for any team
players_max_minutes <- data.frame(players_mult_teams
                                  %>% filter(Tm != 'TOT')
                                  %>% select(Player, MP) 
                                  %>% group_by(Player) 
                                  %>% summarize(max_min = max(MP))
)

# get the team a player played max minutes on
team_max_minutes <- data.frame(
  left_join(
    players_max_minutes, 
    players_mult_teams[ , c('Player', 'MP', 'Tm')],
    # join on max minutes and player name
    by = c('max_min' = 'MP', 'Player')
  )
) 

# assign new team to player
player_assigned_team <- data.frame(
  left_join(
    players_mult_teams 
    %>% select(Player, Tm) 
    %>% filter(Tm == 'TOT'),
    team_max_minutes 
    %>% select(Player, Tm),
    by = 'Player'
  )
)

# get players who were only on one team
players_one_team <- data.frame(nba
                               %>% filter(num_teams == 1)
                               %>% select(Player, Tm)
)

# create copy of team column
players_one_team$Tm_copy <- players_one_team$Tm

# rename column names
setnames(players_one_team, 
         old = names(players_one_team), 
         new = names(player_assigned_team))

# create dataframe of players with their final team assignments
final_teams <- rbind(
  player_assigned_team,
  players_one_team
)

# insert final teams into main data
nba_df <- left_join(nba, final_teams, by = c('Player', 'Tm' = 'Tm.x'))

# drop original team column and number of teams
drop_cols <- c('Tm', 'num_teams')
nba_df <- nba_df[ , -which(names(nba_df) %in% drop_cols)]

# move new team column
nba_df <- add_column(nba_df, nba_df$Tm.y, .after = 'Player')

# rename column
colnames(nba_df)[3] <- 'Tm'

# drop 'NA' rows and redundant team column. These are redundant rows
nba_df <- nba_df[!is.na(nba_df$Tm), -which(names(nba_df) %in% 'Tm.y')]


# Add team salary and Win % to data ---------------------------------------
team_data <- read.csv(filename_team)

# change data type of Team from character to factor
nba_df$Tm <- as.factor(nba_df$Tm)
nba_df <- left_join(nba_df, team_data, by = c('Tm' = 'team'))


# Write to CSV ------------------------------------------------------------


# overwrite file contents
csv_name <- 'season_stats_clean.csv'
# write to csv if file doesn't already exist
#if (!(csv_name %in% list.files('Data'))){
  write.csv(nba_df, paste0('Data/', csv_name), row.names = FALSE)
#}

