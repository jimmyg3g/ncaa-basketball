library(rvest)
# library(magrittr)
library(tidyverse)
library(xml2)

# team_mapping
espnTeamMapping <- function() {
    doc <- read_html('http://www.espn.com/mens-college-basketball/teams')
    team_nodes <- html_nodes(doc, '.medium-logos li')
    espn_team_mapping <- lapply(team_nodes, function(x) {
        team_id <- html_node(x, 'span a') %>% xml_attr('href') %>% gsub('.*=', '', .)
        team_name <- html_nodes(x, 'h5 a') %>% xml_text()
        team_nickname <- html_nodes(x, 'h5 a') %>% xml_attr('href') %>% gsub('.*/', '', .)
        c(team_id=team_id, team_name=team_name, team_nickname=team_nickname)
    })
    espn_team_mapping <- plyr::ldply(espn_team_mapping) %>% tbl_df()
}

espn_team_mapping <- espnTeamMapping()


html_structure(doc)


# schedule_mapping
espnScheduleMapping <- function(team_id, team_nickname) {
    doc <- read_html(paste('http://www.espn.com/mens-college-basketball/team/schedule/_/id', team_id, team_nickname, sep='/'))
    team_schedule <- html_table(doc, header=F)[[1]] %>% tbl_df()
    names(team_schedule) <- team_schedule[2, ] %>% tolower() %>% gsub('/', '_', .)
    team_schedule <- team_schedule[3:nrow(team_schedule), ]
    cbind(team_id=team_id, team_schedule) %>% tbl_df()
}


# ESPN PBP

score_nodes <- html_nodes(doc, 'li.score')
lapply(score_nodes, function(x) { 
    xml_text(x)
    html_node(x, 'a') %>% xml_attr('href')

})

game_nodes <- html_nodes(doc, '[class=game-schedule]')
x <- game_nodes[1]

game_status <- html_nodes(x, '[class=game-status]') %>% html_text()


html_children(game_nodes[[1]])[[3]] %>% html_children()
html_structure(game_nodes[[1]])

espn_schedule_mapping <- espnScheduleMapping(espn_team_mapping$team_id[1], espn_team_mapping$team_nickname[1])


html_nodes(doc, 'tr td') %>% xml_children() %>% xml_attr('class')
html_nodes(doc, 'tr td') %>% xml_children() %>% xml_attr('href')
html_nodes(doc, 'li a') %>% html_structure()

html_nodes(doc, 'tr td ul') %>% xml_children()
html_nodes(doc, 'table tr td')

html_node(doc, 'tr.oddrow') %>% xml_structure()
html_node(doc, 'tr.oddrow') %>% xml_children() %>% xml_attr('td')
html_node(doc, 'tr.oddrow') %>% xml_contents()
html_node(doc, 'tr.oddrow') %>% html_structure()
html_node(doc, 'tr.oddrow') %>% html_structure()


# Do this for tr.oddrow & tr.evenrow
game_nodes <- html_nodes(doc, 'tr.oddrow')
x <- game_nodes[[1]]
game_date <- xml_children(x)[1] %>% xml_text()
game_status <- html_node(x, '[class=game-status]') %>% html_text()
game_score <- html_node(x, '[class=score]') %>% html_text()
game_id <- html_node(x, '[class=score]') %>% html_children() %>% html_attr('href') %>% 
    gsub('.*/', '', .)

# or start here 
html_node(doc, '[class=tablehead]') %>% html_children()
html_node(doc, '[class=tablehead]') %>% html_node('[class="oddrow|evenrow"]')



# ESPN Shots 
doc <- read_html('http://www.espn.com/mens-college-basketball/playbyplay?gameId=400910689')

a <- html_nodes(doc, '[class="shots away-team"]') %>% html_children()
a <- html_nodes(doc, '[class~="shots"]') %>% html_children()
x <- a[[1]]

html_attrs(a, 'class')
