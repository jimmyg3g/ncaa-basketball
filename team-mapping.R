library(rvest)
library(magrittr)
library(dplyr)
library(tidyr)

# team_mappings with team_id, team_name, team_url  ------------------------

url_teams <- "http://stats.ncaa.org/team/inst_team_list?sport_code=MBB&division=1"
selector_teams <- ".css-panes a"

teams_html <- read_html(url_teams)
team_nodes <- html_nodes(teams_html, selector_teams)
 
team_mapping <- lapply(team_nodes, function(x) {
  team_name <- xml_text(x)
  team_link <- xml_attr(x, 'href')
  team_id <- gsub("/team/|/12260", "", team_link)
  team_link <- paste0("http://stats.ncaa.org", team_link)
  c(team_id=team_id, team_name=team_name, team_link=team_link)
  # as.list(c(team_name=team_name, team_link=team_link))
  })
team_mapping <- plyr::ldply(team_mapping)
team_mapping <- tbl_df(team_mapping)


# schedule_mapping with game_id, home_team_id, away_team_id, date, --------
# UNC is team_id 457, row 196


doc <- read_html(team_mapping$team_link[i])

schedule_mapping <- doc %>% html_nodes(".smtext a") %>% xml_attr(., 'href')
schedule_mapping <- schedule_mapping[grepl('game', schedule_mapping)]
schedule_mapping <- gsub(".*index/|\\?.*", "", schedule_mapping)
schedule_mapping <- cbind(team_id = team_mapping$team_id[i], game_id = schedule_mapping) %>% as.data.frame()

 
# xml_text(a[[1]]) # game_date
# xml_text(a[[2]], trim = TRUE) # opponent
# xml_text(a[[3]], trim = TRUE) # score
# html_nodes(a[[5]], 'a') %>% xml_attr('href') # team link
# html_nodes(a[[3]], 'a') %>% xml_attr('href') # game link


doc <- read_html(team_mapping$team_link[i])
doc <- doc %>% html_nodes('.smtext')


schedule_mapping_list <- list()
for (i in seq(1, length(doc), by = 3)) { 
  print(i)
  game_date <- xml_text(doc[[i]])
  opponent <- xml_text(doc[[i+1]], trim = TRUE)
  opponent_id <- html_nodes(doc[[i+1]], 'a') %>% xml_attr('href') %>% 
    gsub(".*team/|/[0-9]{5}$", "", .)
  game_id <- html_nodes(doc[[i+2]], 'a') %>% xml_attr('href') %>% 
    gsub(".*index/|\\?.*", "", .)
  score <- xml_text(doc[[i+2]], trim = TRUE)
  
  # if (grepl('@', opponent)) {
  #   away_team_id <- opponent_id
  #   # home_team_id <- 
}


for (i in 1:length(a)) { 
  print(i)
  if (i %% 3 == 0) xml_text(a[i])
  else print(i)
  }



# schedule table ----------------------------------------------------------

# my_list <- list()
# for (i in 1:3) { 
for (i in 1:nrow(team_mapping)) {
  cat("Start:", i, "\n")
  doc <- read_html(team_mapping$team_link[i])
  doc <- html_table(doc, fill = TRUE)[[2]]
  names(doc) <- doc[2,] %>% stringr::str_to_lower()
  doc <- doc[3:nrow(doc),]
  attr(doc, 'team_id') <- team_mapping$team_id[i]
  attr(doc, 'team_name') <- team_mapping$team_name[i]
  my_list[[team_mapping$team_id[i]]] <- doc
  cat('End:', i, '\n')
  save(my_list, file = 'my_list')
  Sys.sleep(3)
}

schedule_table <- my_list

schedule_table <- plyr::ldply(schedule_table)
schedule_table <- tbl_df(schedule_table)
names(schedule_table) <- c('team_id', names(schedule_table)[2:4])

schedule_table <- separate(schedule_table, result, c('win_loss', 'other'), sep = " ", extra = 'merge')
schedule_table <- separate(schedule_table, other, c('team_score', 'opp_score'), sep = " \\- ", extra = 'drop')
schedule_table <- separate(schedule_table, opp_score, c('opp_score', 'ot'), sep = " ", extra = 'merge', fill = 'right')

schedule_table$team_score <- schedule_table$team_score %>% as.numeric()
schedule_table$opp_score <- schedule_table$opp_score %>% as.numeric()
schedule_table$margin_of_victory <- schedule_table$team_score - schedule_table$opp_score



tidyr::separate(schedule_table, result, c('win_loss', 'team_score', 'trash', 'opp_score', 'ot', sep = " ", fill = 'warn'))

schedule_table <- lapply(schedule_table, function(x) { 
  x <- tidyr::separate(x, result, c('win_loss', 'team_score', 'trash', 'opp_score'), sep = " ", extra = 'drop')
  x <- x %>% select(-trash)
  # x$team_score <- x$team_score %>% as.numeric()
  # x$opp_score <- x$opp_score %>% as.numeric()
  # x$margin_of_victory <- x$team_score - x$opp_score
})

for (i in team_mapping$team_id) { 
  schedule_table[[i]] <- tidyr::separate(schedule_table[[i]], result, c('win_loss', 'team_score', 'trash', 'opp_score'), sep = " ", extra = 'drop')
  schedule_table[[i]] <- schedule_table[[i]] %>% select(-trash)
  schedule_table[[i]]$team_score <- schedule_table[[i]]$team_score %>% as.numeric()
  schedule_table[[i]]$opp_score <- schedule_table[[i]]$opp_score %>% as.numeric()
  schedule_table[[i]]$margin_of_victory <- schedule_table[[i]]$team_score - schedule_table[[i]]$opp_score
}

lapply(schedule_table, function(x) { 
  attributes(x)
})

lapply(schedule_table, function(x) { 
  cat(min(x$margin_of_victory), "\n")
})








gsub(".*@ ", "", schedule_table$opponent)

date <- doc %>% html_nodes('.smtext:nth-child(1)') %>% xml_text()

x <- doc %>% html_nodes('.smtext')
game_date <- for (i in seq(1, length(x), 3)) x[i] %>% xml_text() 
x %>% xml_children()
x %>% xml_text()

# schedule mapping try doc %>% html_nodes('.smtext'), seq start1 by 3 is the loop for info




# player_mapping
doc <- read_html(paste0('http://stats.ncaa.org/team/', team_mapping$team_id[i], '/roster/12260'))
player_mapping <- html_table(doc, header = FALSE)[[1]]
names(player_mapping) <- player_mapping[2,] %>% stringr::str_to_lower()
player_mapping <- player_mapping[3:nrow(player_mapping),]
player_mapping <- cbind(team_id = team_mapping$team_id[i], player_mapping)

# get box score
doc <- read_html(paste0('http://stats.ncaa.org/game/index/', schedule_mapping$game_id[i]))
html_nodes(doc, 'td')

html_table(doc)

# get play by play
doc <- read_html(paste0('http://stats.ncaa.org/game/play_by_play/', schedule_mapping$game_id[i]))
play_by_play <- html_nodes(doc, '.mytbl+ .mytable td')

pbp <- xml_text(play_by_play)
my_header <- pbp[1:4]
pbp[nchar(pbp) == 0] <- " "

my_list <- list()
for (i in seq(5, length(pbp)-4, 4)) {
  print(i) 
  my_list[[length(my_list)+1]] <- pbp[i:(i+3)]
}
pbp <- plyr::ldply(my_list)
names(pbp) <- my_header


left_join(schedule_table %>% group_by(team_id) %>% summarise(max(team_score)), team_mapping) %>% 
  left_join(kp, .) %>% filter(seed == '1')
