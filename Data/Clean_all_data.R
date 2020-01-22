# script cleans the raw stats data then scrapes and merges the salaries and 
#   RPM data. Final output is the 'season_stats_clean.csv'
source('Data/Clean_stats_data.R')
source('Data/Scrape_salaries.R')
source('Data/Scrape_RPM.R')