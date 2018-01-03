pacman::p_load(tidyverse, xml2, rvest, lubridate, stringr, ggplot2, hexbin, grid, RCurl, jpeg)

# cbs base_url 'http://www.cbssports.com/'


# CBS Team Mapping --------------------------------------------------------

cbsTeamMapping <- function() { 
    doc <- read_html('http://www.cbssports.com/college-basketball/teams/')
    team_nodes <- html_nodes(doc, 'td a')
    output <- lapply(team_nodes, function(x) { 
        team_name <- xml_text(x)
        team_abbreviation <- xml_attr(x, 'href') %>% gsub('.*page/', '', .) %>% gsub('/.*', '', .)
        team_nickname <- xml_attr(x, 'href') %>% gsub('.*/', '', .)
        c(team_name=team_name, team_abbreviation=team_abbreviation, team_nickname=team_nickname)
    })
    plyr::ldply(output) %>% tbl_df()
}

cbs_team_mapping <- cbsTeamMapping()

cbs_unc <- filter(cbs_team_mapping, team_abbreviation=='UNC')


# CBS Schedule Mapping ----------------------------------------------------
httr::BROWSE('http://www.cbssports.com/collegebasketball/teams/schedule/UNC/north-carolina-tar-heels')

cbsScheduleMapping <- function(team_abbreviation, team_nickname) {
    doc <- read_html(paste('http://www.cbssports.com/collegebasketball/teams/schedule', team_abbreviation, team_nickname, sep='/'))
    output <- html_table(doc)[[3]] %>% tbl_df()
    names(output) <- output[3, ] %>% tolower() %>% gsub(' / ', '_', .)
    output <- output[4:nrow(output), ]
}

# Get game_recap links.
doc <- read_html('http://www.cbssports.com/collegebasketball/teams/schedule/UNC/north-carolina-tar-heels')

# doc <- read_html('http://www.cbssports.com/collegebasketball/teams/roster/UNC/north-carolina-tar-heels')
# html_nodes(doc, '[class="data"]')
game_recaps <- html_nodes(doc, 'body') %>% html_children %>% html_nodes('td a') %>% html_attr('href')
game_recaps <- game_recaps[str_detect(game_recaps, 'recap')] %>% paste0('http://www.cbssports.com', .)
game_recaps <- gsub('/recap/', '/live/', game_recaps)

docs <- lapply(game_recaps, read_html)


# TODO: Generate game links
'http://www.cbssports.com/collegebasketball/gametracker/playbyplay/NCAAB_20161111_UNC@TULANE'
'http://www.cbssports.com/collegebasketball/gametracker/playbyplay/NCAAB_20161111_BROWN@CINCY'
unc_schedule <- cbsScheduleMapping(cbs_unc$team_abbreviation, cbs_unc$team_nickname)
unc_schedule <- unc_schedule %>% mutate(date=ifelse(grepl('Nov|Dec', date), paste(date, '2016', sep=', '), paste(date, '2017', sep=', '))) %>% 
    mutate(date=mdy(date) %>% as.character() %>% gsub('\\-', '', .)) %>% 
    mutate(opponent_name=gsub('@|vs. ', '', opponent) %>% gsub('St\\.', 'State', .) %>% gsub('Va\\.', 'Virginia', .))



left_join(unc_schedule, cbs_team_mapping, by=c('opponent_name' = 'team_name')) %>% View()
          

base_url <- 'http://www.cbssports.com/collegebasketball/gametracker/playbyplay/NCAAB'



'http://www.cbssports.com/collegebasketball/gametracker/playbyplay/NCAAB_20161111_UNC@TULANE'
'http://www.cbssports.com/collegebasketball/gametracker/playbyplay/NCAAB_20161111_BROWN@CINCY'

# Format for game data.
'http://www.cbssports.com/collegebasketball/gametracker/playbyplay/NCAAB_', game_date, away_team_abbreviation, '@', home_team_abbreviation



# CBS Shot Location Data --------------------------------------------------

cbsShotLocationData <- function(doc) { 
    # doc <- read_html('http://www.cbssports.com/collegebasketball/gametracker/live/NCAAB_20161217_UK@UNC')
    # doc <- read_html('http://www.cbssports.com/collegebasketball/gametracker/live/NCAAB_20170121_UNC@BC#') # this has tons of shot data
    # 
    # http://sports.cbsimg.net/js/CBSi/app/LiveChartNcaab-v005.js
    # This works for scoring data! 
    my_nodes <- html_nodes(doc, '[class="liveChart"]') %>% html_children()
    if (length(my_nodes) > 0 ) { 
        my_text <- my_nodes[5] %>% html_text(trim=TRUE)
        my_text <- my_text %>% gsub('\\s+', '', .) %>% gsub('.*shotData:', '', .) %>% gsub('~', '\n', .) %>% 
            gsub(',awayScoringData.*', '', .) %>% gsub('"', '', .)
        scoring_data <- str_split(my_text, '\n') %>% data.frame(stringsAsFactors=F) %>% tbl_df()
        names(scoring_data) <- 'V1'
        
        scoring_data <- separate(scoring_data, V1, c('team', 'time', 'period', 'player_id', 'shot_type', 'result',
                                                     'loc_x', 'loc_y', 'distance'), sep = ',')    
        
        scoring_data$loc_x <- as.numeric(scoring_data$loc_x)
        scoring_data$loc_y <- as.numeric(scoring_data$loc_y) %>% abs()
        scoring_data$distance <- as.numeric(scoring_data$distance)
        return(scoring_data)
    }
}

shots <- list()
for(i in 1:length(docs)) { 
    cat(i, '\n')
    shots[[i]] <- cbsShotLocationData(docs[[i]])
}

shots <- lapply(docs, cbsShotLocationData)

scoring_data <- plyr::ldply(shots) %>% tbl_df()

# scoring_data$result <- as.numeric(scoring_data$result)

# TODO: figure out column names, better way than writing to .txt file?  
# Columns
# v1: away==0, home==1 
# v2: time remaining 
# v3: period/half
# v4: player_id 
# v5: shot_type
# v6: result 0==missed, 1==made 
# v7: x_position negative is left side of court.
# v8: y_position distance from center court in feet.
# v9: distance



scoring_data %>% filter(grepl('11|12', shot_type))
scoring_data %>% filter(!grepl('11|12', shot_type))

scoring_data %>% filter(!grepl('11|12', shot_type)) %>% 
    ggplot(., aes(loc_x, -loc_y+42, colour=result)) + 
    geom_count()

# Joel Berry
scoring_data %>% filter(player_id=='2152684' & !grepl('11|12', shot_type))
scoring_data %>% filter(player_id=='2152684' & !grepl('11|12', shot_type)) %>% 
    ggplot(., aes(-loc_y+42, loc_x, colour=as.factor(result))) + geom_point()
    
scoring_data %>% filter(player_id=='2152684' & !grepl('11|12', shot_type)) %>% 
    ggplot(., aes(-loc_x, -loc_y+42, colour=as.factor(result))) + 
    geom_point() +

scoring_data %>% filter(!grepl('11|12', shot_type)) %>% 
    ggplot(., aes(-loc_x, -loc_y+42, colour=as.factor(result))) + geom_point()

scoring_data %>% filter(!grepl('11|12', shot_type) & result=='1') %>% 
    ggplot(., aes(-loc_x, -loc_y+42)) + 
    stat_binhex(colour='gray', alpha=0.7) + 
    scale_fill_gradientn(colours=c('yellow', 'orange', 'red'))

# Info on locking size of graph to court.
# https://thedatagame.com.au/2015/09/27/how-to-create-nba-shot-charts-in-r/ 


# half court image
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

courtImg.URL <- "C:/Users/jglenn.CNHIDOM/Downloads/NCAAGT2014court-lines.jpeg"
court <- rasterGrob(readJPEG(courtImg.URL), 
                    width=unit(1, 'npc'), height=unit(1, 'npc'))

# scoring_data %>% filter(!grepl('11|12', shot_type)) %>% 
scoring_data %>% filter(player_id=='2152684' & !grepl('11|12', shot_type)) %>% 
    ggplot(., aes(-loc_x, -loc_y+42, colour=result, shape=result)) + 
    annotation_custom(court, -25, 25, -5, 42) +
    geom_point() +
    xlim(-25, 25) + 
    ylim(-0, 42) + 
    geom_rug(alpha=0.2) + 
    coord_fixed()


# court background http://sports.cbsimg.net/images/collegebasketball/gametracker/NCAAGT2014court-bg.png
# court lines: url('http://sports.cbsimg.net/images/collegebasketball/gametracker/NCAAGT2014court-lines.png');

library(png)
'http://a.espncdn.com/redesign/assets/img/ncaab/bg-court-logo.png'
court_background_url <- 'http://sports.cbsimg.net/images/collegebasketball/gametracker/NCAAGT2014court-bg.png'
court_lines_url <- 'http://sports.cbsimg.net/images/collegebasketball/gametracker/NCAAGT2014court-lines.png'

court_background <- rasterGrob(readPNG(getURLContent(court_background_url)),
           width=unit(1,"npc"), height=unit(1,"npc"))

court_lines <- rasterGrob(readPNG(getURLContent(court_lines_url)),
                               width=unit(1,"npc"), height=unit(1,"npc"))


scoring_data %>% filter(!grepl('11|12', shot_type)) %>% 
    ggplot(., aes(-loc_x, -loc_y+42, colour=as.factor(result))) + 
    annotation_custom(court_background, -25, 25, -5, 42) 
    geom_point() + 
    xlim(-25, 25) + 
    ylim(-0, 42) + 
    geom_rug(alpha=0.2) + 
    coord_fixed()

    
court_lines <- readPNG("C:/Users/jglenn.CNHIDOM/Downloads/NCAAGT2014court-lines.png")
    