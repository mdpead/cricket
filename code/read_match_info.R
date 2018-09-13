library(tidyverse)
library(rvest)

read_match_info <- function(series_id, game_id){
  
  url <- paste0("http://www.espncricinfo.com/series/", series_id, "/game/", game_id)

  page <- tryCatch(read_html(url), error = function(c) break)
  
  team_one <- page %>%
    html_nodes(css = ".table-wrap") %>%
    as_list() %>%
    .[[1]] %>%
    .[[1]] %>%
    .$caption %>%
    .[[2]] %>%
    stringr::str_trim(.)
  
  team_two <- page %>%
    html_nodes(css = ".table-wrap") %>%
    as_list() %>%
    .[[2]] %>%
    .[[1]] %>%
    .$caption %>%
    .[[2]] %>%
    stringr::str_trim(.)

squad_one <- page %>% 
    html_nodes(css = "#main-container > div > div.layout-bc > div.col-b > div.inner-wrapper > div > div.col-small.inner > div.hide-show > article > div:nth-child(2) > table") %>% 
    html_table(.) %>% .[[1]] %>% .[[1]] %>%
  stringr::str_replace(fixed(" (c)"), "") %>%
  stringr::str_replace(fixed(" \U2020"), "")
    
squad_two <- page %>% 
  html_nodes(css = "#main-container > div > div.layout-bc > div.col-b > div.inner-wrapper > div > div.col-small.inner > div.hide-show > article > div:nth-child(3) > table") %>% 
  html_table(.) %>% .[[1]] %>% .[[1]] %>%
  stringr::str_replace(fixed(" (c)"), "") %>%
  stringr::str_replace(fixed(" \U2020"), "")

ground <- page %>%
  html_nodes(css = "#main-container > div > div.layout-bc > div.col-b > div.inner-wrapper > div > div.col-small.inner > div:nth-child(3) > article > div > div > div.stadium-details > h4 > a > span") %>%
  as_list(.) %>%
  .[[1]] %>%
  .[[1]]
  
start_date <- Sys.Date()
  
 format <- page %>%
    html_node(css = "#main-container > div > div.layout-bc > div.col-b > div.inner-wrapper > div > div.col-small.inner > div:nth-child(3) > article > div > div > div:nth-child(3) > div.match-detail--right > span > a") %>%
       as_list(.) %>%
    .[[1]] %>%
    str_extract("^([\\w\\-]+)")
  
  return(list(
    mt_start_date = start_date,
    mt_format = format,
    mt_ground= ground,
    mt_team_one = team_one,
    mt_squad_one = squad_one,
    mt_team_two = team_two,
    mt_squad_two = squad_two
  ))
  
}