library(rvest)
library(magrittr)
library(dplyr)
library(tidyr)
library(xml2)


# get play by play
# TO DO: turn this garbage into a function that uses the length/number of tables to loop through OT play
# http://stats.ncaa.org/game/play_by_play/4070331 is a double OT game

# doc <- read_html('http://stats.ncaa.org/game/play_by_play/4070331')

doc <- read_html(paste0('http://stats.ncaa.org/game/play_by_play/', unc_schedule$game_id[i]))


pbp <- get_pbp(unc_schedule$game_id[i])
    


pbp <- html_nodes(doc, '.mytable')[[5]] %>% html_table(header=TRUE) %>% tbl_df()

html_table(doc)[[1]]


doc <- read_html(paste0('http://stats.ncaa.org/game/play_by_play/', unc_schedule$game_id[i]))
html_nodes(doc, '.mytable')[[1]] %>% html_table() # score by half

pbp1 <- html_nodes(doc, '.mytable')[[2]] %>% html_table(header=T) # play by play 1st half
names(pbp1) <- pbp1[1,]
pbp1 <- pbp1[2:nrow(pbp1),] %>% tbl_df()

pbp2 <- html_nodes(doc, '.mytable')[[3]] %>% html_table() # play by play 2nd half
names(pbp2) <- pbp2[1,]
pbp2 <- pbp2[2:nrow(pbp2),] %>% tbl_df()

html_nodes(doc, '.mytable') %>% html_table()

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

