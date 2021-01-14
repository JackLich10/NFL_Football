library(tidyverse)
library(gt)
library(future)
library(ggtext)
library(ggpmisc)

# Change working directory
setwd("~/Desktop/RStudio/NFL_Football/fourth_downs")
source('https://raw.githubusercontent.com/guga31bb/fourth_calculator/main/R/helpers.R')
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_nflscrapr_mutations.R")
source('https://raw.githubusercontent.com/guga31bb/fourth_calculator/main/R/season_numbers_functions.R')

# Get current season decisions
fourth_downs2020 <- get_season(2020)

# Bind with prior seasons
fourth_downs <- readRDS("prior_season_decisions.rds") %>% 
  bind_rows(., fourth_downs2020) %>% 
  mutate(season = as.integer(str_sub(game_id, end = 4)),
         week = as.integer(str_sub(game_id, start = 6, end = 7)))

# Return a team's ESPN logo
ESPN_logo_url <- function(x) {
  ifelse(is.na(x), NA, paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
}

# Initialize team colors
NFL_pri <- c('ARI'='#97233f',
             'ATL'='#a71930',
             'BAL'='#241773',
             'BUF'='#00338d',
             'CAR'='#0085ca',
             'CHI'='#0b162a',
             'CIN'='#000000',
             'CLE'='#fb4f14',
             'DAL'='#002244',
             'DEN'='#002244',
             'DET'='#005a8b',
             'GB'='#203731',
             'HOU'='#03202f',
             'IND'='#002c5f',
             'JAX'='#000000',
             'KC'='#e31837',
             'LAC'='#002244',
             'LA'='#003693',
             'MIA'='#008e97',
             'MIN'='#4f2683',
             'NE'='#002244',
             'NO'='#9f8958',
             'NYG'='#0b2265',
             'NYJ'='#125740',
             'OAK'='#a5acaf',
             'LV'='#a5acaf',
             'PHI'='#004953',
             'PIT'='#000000',
             'SF'='#aa0000',
             'SEA'='#002244',
             'TB'='#d50a0a',
             'TEN'='#002244',
             'WAS'='#773141')


NFL_sec <- c('pos'='#FFFFFF',
             'neg'='#000000',
             'ARI'='#000000',
             'ATL'='#000000',
             'BAL'='#000000',
             'BUF'='#c60c30',
             'CAR'='#000000',
             'CHI'='#c83803',
             'CIN'='#fb4f14',
             'CLE'='#22150c',
             'DAL'='#b0b7bc',
             'DEN'='#fb4f14',
             'DET'='#b0b7bc',
             'GB'='#ffb612',
             'HOU'='#a71930',
             'IND'='#a5acaf',
             'JAX'='#006778',
             'KC'='#ffb612',
             'LAC'='#0073cf',
             'LA'='#ffd000',
             'MIA'='#f58220',
             'MIN'='#ffc62f',
             'NE'='#c60c30',
             'NO'='#000000',
             'NYG'='#a71930',
             'NYJ'='#000000',
             'OAK'='#000000',
             'LV'='#000000',
             'PHI'='#a5acaf',
             'PIT'='#ffb612',
             'SF'='#b3995d',
             'SEA'='#69be28',
             'TB'='#34302b',
             'TEN'='#4b92db',
             'WAS'='#ffb612')
