
library(rvest)
library(magrittr)
library(dplyr)
library(tidyr)
library(xml2)

academic_year <- '2017'
year_index <- '12480'
base_url <- 'http://stats.ncaa.org'

team_mappings <- create_team_mapping(academic_year, year_index)
write.csv(team_mappings, file = './mappings/team_mappings.csv')

# Grabbing UNC's schedule mapping...
i <- team_mappings %>% filter(team_name=='North Carolina') %>% select(index) %>% as.numeric()
# schedule_mapping <- create_schedule_mapping(team_mapping$team_id[i], year_index)


schedule_mappings <- list()
i <- 1
for(x in team_mappings$team_id[142:351]) { 
    cat(i, 'of', length(team_mappings$team_id), '\n')
    schedule_mappings[[x]] <- create_schedule_mapping(x, year_index)
    i <- i + 1 
}


plyr::ldply(schedule_mappings) %>% write.csv(., file = './mappings/schedule_mappings.csv')
# TODO: add 'Processing team 688 (93 of 351)' feedback in the console.
schedule_mappings <- plyr::ldply(schedule_mappings) %>% tbl_df()

unc_schedule <- schedule_mappings[[i]] %>% tbl_df()


player_mappings <- list()
i <- 1
for(x in team_mappings$team_id) { 
    cat(i, 'of', length(team_mappings$team_id), '\n')
    player_mappings[[x]] <- create_player_mapping(x, year_index)
    i <- i + 1
}

plyr::ldply(player_mappings) %>% write.csv(., file = './mappings/player_mappings.csv')
player_mappings <- plyr::ldply(player_mappings) %>% tbl_df()


pbp <- 


