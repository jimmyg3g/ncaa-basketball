pacman::p_load(tidyverse, rvest, tidyr, xml2, lubridate)

link <- "https://www.cbssports.com/collegebasketball/gametracker/live/NCAAB_20171126_UNC@MICHST"
httr::BROWSE(link)

doc <- read_html(link)

html_nodes(doc, ".liveChart") %>% html_children()
html_nodes(doc, css = ".liveChart") %>% html_children()

shot_data <- html_nodes(doc, css = ".liveChart > script:last-child") %>%
  html_text() %>%
  str_replace_all(., "\\n", "") %>%
  str_replace_all(., '.*shotData: \"', "") %>%
  str_replace_all(., '\".*', "") %>%
  str_split(., "~", simplify = F) %>%
  unlist() %>%
  as_tibble() %>%
  separate(value, c("team", "time", "period", "player", "type", "result", "x_pos", "y_pos", "distance"), sep = "\\,") %>%
  mutate_at(vars(team, period:distance), funs(as.numeric)) 
  # %>% mutate(time2 = ms(time))
# team, time, period, player_id, type, result, x_pos, y_pos, distance

# team 0 is away, team 1 is home

away_scoring_data <- html_nodes(doc, css = ".liveChart > script:last-child") %>%
  html_text() %>%
  str_replace_all(., "\\n", "") %>%
  str_replace_all(., '.*awayScoringData: \"', "") %>%
  str_replace_all(., '\".*', "") %>%
  str_split(., "~|\\|", simplify = F) %>%
  unlist() %>%
  as_tibble() %>%
  separate(value, c("id", "name", "number", "position", "fg", "3fg", "ft", "pts"), sep = ":|\\,") %>%
  mutate_at(vars(id, number, pts), funs(as.numeric)) %>%
  mutate(name = str_replace(name, "&nbsp;", " "))


home_scoring_data <- html_nodes(doc, css = ".liveChart > script:last-child") %>%
  html_text() %>%
  str_replace_all(., "\\n", "") %>%
  str_replace_all(., '.*homeScoringData: \"', "") %>%
  str_replace_all(., '\".*', "") %>%
  str_split(., "~|\\|", simplify = F) %>%
  unlist() %>%
  as_tibble() %>%
  separate(value, c("id", "name", "number", "position", "fg", "3fg", "ft", "pts"), sep = ":|\\,") %>%
  mutate_at(vars(id, number, pts), funs(as.numeric)) %>%
  mutate(name = str_replace(name, "&nbsp;", " "))

home_team <- html_nodes(doc, css = ".liveChart > script:last-child") %>%
  html_text() %>%
  str_extract(., "homeTeamAbbr.*\\,") %>%
  str_extract(., "\\'.*\\'") %>%
  str_replace_all(., "\\'", "")

away_team <- html_nodes(doc, css = ".liveChart > script:last-child") %>%
  html_text() %>%
  str_extract(., "awayTeamAbbr.*\\,") %>%
  str_extract(., "\\'.*\\'") %>%
  str_replace_all(., "\\'", "")



shot_types <- data.frame(
  stringsAsFactors = FALSE,
  value = c(
    "case 1: return \"Jump Shot\"",
    "case 2: return \"Running Jump\"",
    "case 3: return \"Hook Shot\"", "case 4: return \"Tip-in\"",
    "case 5: return \"Layup\"",
    "case 6: return \"Driving Layup\"", "case 7: return \"Dunk Shot\"",
    "case 8: return \"Slam Dunk\"", "case 9: return \"Driving Dunk\"",
    "case 10: return \"Free Throw\"",
    "case 11: return \"1st of 2 Free Throws\"",
    "case 12: return \"2nd of 2 Free Throws\"",
    "case 13: return \"1st of 3 Free Throws\"",
    "case 14: return \"2nd of 3 Free Throws\"", "case 15: return \"3rd of 3 Free Throws\"",
    "case 16: return \"Technical Free Throw\"",
    "case 17: return \"1st of 2 Free Throws\"",
    "case 18: return \"2nd of 2 Free Throws\"",
    "case 19: return \"Finger Roll\"", "case 20: return \"Reverse Layup\"",
    "case 21: return \"Turnaround Jump Shot\"",
    "case 22: return \"Fadeaway Jump Shot\"",
    "case 23: return \"Floating Jump Shot\"",
    "case 24: return \"Leaning Jump Shot\"", "case 25: return \"Baby Hook Shot\"",
    "case 26: return \"Alley-oop\""
  )
) %>%
  tbl_df() %>%
  mutate(
    type = str_replace_all(value, "case ", "") %>% str_replace(., ":.*", "") %>% as.numeric(),
    description = str_replace_all(value, ".*return ", "") %>%
      str_replace_all(., "\\W", " ") %>%
      str_trim()
  ) %>%
  select(-value)


shot_data %>% left_join(shot_types) %>% count(description)
shot_data %>%
  left_join(shot_types) %>%
    filter(!grepl('free', tolower(description))) %>% 
  group_by(team) %>%
  # count(description, result) %>%
    count(result) %>% 
  mutate(result = case_when(
    result == 0 ~ "miss",
    result == 1 ~ "make",
    TRUE ~ "fubar"
  )) %>%
  spread(result, n) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  mutate(
    fga = make + miss,
    fg_pct = make / fga
  )

shot_data %>% 
    count(team, tre = distance >= 20.75)


d %>% left_join(shot_types) %>% filter(grepl('Jump', description)) %>% 
    mutate(y_pos = abs(y_pos)) %>% 
    ggplot(aes(x=x_pos, y=y_pos)) + geom_point(aes(colour = result))


library(png)
'http://a.espncdn.com/redesign/assets/img/ncaab/bg-court-logo.png'
court_background_url <- 'http://sports.cbsimg.net/images/collegebasketball/gametracker/NCAAGT2014court-bg.png'
court_lines_url <- 'http://sports.cbsimg.net/images/collegebasketball/gametracker/NCAAGT2014court-lines.png'

court_background <- rasterGrob(readPNG(getURLContent(court_background_url)),
                               width=unit(1,"npc"), height=unit(1,"npc"))

court_lines <- rasterGrob(readPNG(getURLContent(court_lines_url)),
                          width=unit(1,"npc"), height=unit(1,"npc"))



shot_data %>% filter(player == 2152684) %>% 
    ggplot(aes(x_pos, y_pos, colour = as.factor(result))) + 
    geom_point() + 
    annotation_custom(court_lines, -25, 25, -5, 42)
