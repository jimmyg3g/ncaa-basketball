pacman::p_load(rvest, magrittr, tidyverse, xml2)

# library(rvest)
# library(magrittr)
# library(dplyr)
# library(tidyr)
# library(xml2)

academic_year <- '2017'
year_index <- '12480'
base_url <- 'http://stats.ncaa.org'


# create team mapping function --------------------------------------------

create_team_mapping <- function(academic_year, year_index) { 
  search_url <- paste0(base_url, '/team/inst_team_list?sport_code=MBB&division=1&academic_year=', 
                        academic_year, '&division=1')
  doc <- read_html(search_url)
  team_nodes <- html_nodes(doc, '.css-panes a')
  team_mapping <- lapply(team_nodes, function(x) {
    team_name <- xml_text(x)
    team_url <- xml_attr(x, 'href')
    team_id <- team_url %>% gsub(paste0(year_index, '$'), '', .) %>% gsub('/|team', '', .)
    team_url <- paste0("http://stats.ncaa.org", team_url)
    c(team_id=team_id, team_name=team_name, team_url=team_url)
    # as.list(c(team_name=team_name, team_url=team_url))
  })
  team_mapping <- plyr::ldply(team_mapping)
  team_mapping <- tbl_df(team_mapping)
  team_mapping$index <- 1:nrow(team_mapping)
  team_mapping <- team_mapping %>% select(index, team_id, team_name, team_url)
}



# create schedule mapping -------------------------------------------------
# game_id, home_team_id, away_team_id, date, neutral_site, game_link
# UNC is team_id 457, row 196
# team_mapping %>% filter(team_id == 457)

create_schedule_mapping <- function(team_id, year_index) {
  search_url <- paste(base_url, 'team', team_id, year_index, sep = '/')
  doc <- read_html(search_url) 
  
  my_table <- html_table(doc, fill = TRUE)[[2]]
  names(my_table) <- my_table[2,] %>% stringr::str_to_lower()
  my_table <- my_table[3:nrow(my_table),]
  row.names(my_table) <- NULL
  my_table <- my_table %>% tbl_df()
  
  doc <- html_nodes(doc, '.smtext')
  my_list <- list()
  for (i in seq(1, length(doc), by = 3)) { 
    game_date <- xml_text(doc[[i]])
    opponent <- xml_text(doc[[i+1]], trim = TRUE)
    opponent_id <- html_nodes(doc[[i+1]], 'a') %>% xml_attr('href') %>% 
      gsub(".*team/|/[0-9]{5}$", "", .)
    opponent_id <- ifelse(length(opponent_id)==0, 'unknown', opponent_id)
    
    if (grepl('@', opponent)) {
      home_team_id <- opponent_id
      away_team_id <- team_id
      } else { 
      home_team_id <- team_id
      away_team_id <- opponent_id
      }
    
    neutral_site <- ifelse(grepl(',', opponent), 1, 0)

    game_link <- html_nodes(doc[[i+2]], 'a') %>% xml_attr('href')
    game_link <- ifelse(length(game_link)==0, 'unknown', game_link)
    game_id <- gsub(".*index/|\\?.*", "", game_link)
    
    my_list[[length(my_list)+1]] <- c(game_id = game_id, game_date = game_date, home_team_id = home_team_id, 
                                      away_team_id = away_team_id, neutral_site = neutral_site, game_link = game_link)
  }
  schedule_mapping <- do.call(rbind, my_list) %>% as.data.frame(stringsAsFactors = FALSE) %>% tbl_df()
  # list(schedule_mapping = schedule_mapping, schedule_table = my_table)
}



# create player mapping ---------------------------------------------------

create_player_mapping <- function(team_id, year_index) {
  search_url <- paste(base_url, 'team', team_id, 'roster', year_index, sep = '/')
  doc <- read_html(search_url)
  
  player_mapping <- html_table(doc, header = FALSE)[[1]]
  names(player_mapping) <- player_mapping[2,] %>% stringr::str_to_lower()
  player_mapping <- player_mapping[3:nrow(player_mapping),]
  player_mapping <- cbind(team_id = team_id, player_mapping, stringsAsFactors = FALSE, 
                          row.names = NULL)
  
  player_id <- html_nodes(doc, 'td') %>% xml_children() %>% xml_attr('href')
  player_id <- gsub('.*seq=', '', player_id)
  player <- html_nodes(doc, 'td') %>% xml_children() %>% xml_text()
  players <- cbind(player, player_id) %>% as.data.frame(stringsAsFactors = FALSE, row.names = NULL) %>% tbl_df()
  
  player_mapping <- left_join(player_mapping, players, by = 'player')
}



# TODO: get_box_score get_play_by_play get_individual_stats get_aggregate_stats
get_pbp <- function(game_id) { 
    cat('Retrieving PBP: ', game_id, '\n')
    doc <- read_html(paste0('http://stats.ncaa.org/game/play_by_play/', game_id))
    pbp <- list()
    for(i in 2:(html_nodes(doc, '.mytable') %>% html_table() %>% length())) { 
        pbp[[i-1]] <- html_nodes(doc, '.mytable')[[i]] %>% html_table(header=TRUE) %>% tbl_df() %>% mutate(period=i-1, game_id=game_id)
    }
    pbp <- plyr::ldply(pbp) %>% tbl_df()
    names(pbp) <- names(pbp) %>% tolower() %>% gsub(' ', '_', .)
    pbp <- pbp %>% select(game_id, period) %>% left_join(., pbp, by = c('game_id', 'period'))
    return(pbp)
}


