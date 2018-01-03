
# play with my data -------------------------------------------------------

kp$region <- gsub("[0-9]", "", kp$seed_region)
kp$seed <- gsub("[A-Z]", "", kp$seed_region)
kp <- kp %>% select(region, seed, seed_region, team_name)

my_data <- left_join(team_mapping, schedule_table, by = 'team_id')
my_data <- my_data %>% select(-team_link) %>% select(team_id, team_name, date, opponent, win_loss, ot, 
                                                     team_score, opp_score, margin_of_victory)

my_data <- my_data[which(!is.na(my_data$margin_of_victory)),]


# make bracquet.csv file --------------------------------------------------

my_data %>% group_by(team_name, team_id) %>% summarise(max_loss = min(margin_of_victory), max_win = max(margin_of_victory), median_win = median(margin_of_victory)) %>% 
  arrange(-max_loss) %>% left_join(kp, ., by = 'team_name') %>% write.csv('bracquet.csv', row.names = FALSE)
my_data %>% group_by(team_name) %>% summarise(median_victory = median(margin_of_victory)) %>% arrange(-median_victory) %>% View()
my_data[!is.na(my_data$margin_of_victory)]

schedule_table %>% group_by(team_id) %>% summarise(min(margin_of_victory))

kp <- kenpom_predictions %>% select(Seed_Region, Team)
names(kp) <- c('seed_region', 'team_name')

left_join(kp, my_data) %>% distinct(team_name)%>% select(team_name, team_id, seed_region) %>% filter(is.na(team_id))

kp$team_name <- sub('Miami FL', 'Miami (FL)', kp$team_name) %>% sub('Connecticut', 'UConn',. ) %>% sub('S. F. Austin', 'SFA', .) %>% 
  sub('USC', 'Southern California', .) %>% sub('UNC Wilmington', 'UNCW', .) %>% sub('Northern Iowa', 'UNI', .)






