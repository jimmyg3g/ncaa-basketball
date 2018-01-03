pacman::p_load(tidyverse, rvest, tidyr, xml2)

# espn shot data ----------------------------------------------------------
httr::BROWSE('http://www.espn.com/mens-college-basketball/playbyplay?gameId=401001005')
doc <- read_html('http://www.espn.com/mens-college-basketball/playbyplay?gameId=401001005')

d <- doc %>% html_nodes(css = '.shots') %>% html_children()
x <- map(d, function(x) { 
    list(
        id = html_attr(x, 'id'),
        text = html_text(x),
        class = html_attr(x, 'class'),
        team = html_attr(x, 'data-homeaway'),
        period = html_attr(x, 'data-period'),
        shooter = html_attr(x, 'data-shooter'),
        style = html_attr(x, 'style')
    ) %>% as_tibble() 
}) %>% plyr::ldply() %>% tbl_df()


x %>%  {
    bind_rows(
        filter(., class == 'made') %>% 
            separate(style, c('border_color', 'background_color', 'left', 'top', 'end'), sep = ';'), 

        filter(., class == 'missed') %>% 
            separate(style, c('border_color', 'left', 'top', 'end'), sep = ';')
        )
} %>% 
    mutate_at(vars(border_color, background_color, left, top), funs(str_replace_all(., '.*:', ''))) %>% 
    mutate_at(vars(left, top), funs(str_replace_all(., '%', '') %>% as.numeric())) %>% 
    filter(grepl('langford', tolower(text)))
