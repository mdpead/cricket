library(tidyverse)

highest_batsman_prediction <- function(match_details){
  
  
# Innings

  innings <- list(
    
    as_tibble(match_details[names(match_details) != "mt_squad_two"]) %>%
    mutate(id = row_number(),
           mt_id = 0,
           mt_team = mt_team_one,
           mt_opposition = mt_team_two,
           pl_name = mt_squad_one,
           in_no = 1) %>%
    select(-mt_team_one, -mt_team_two, -mt_squad_one),
    
    as_tibble(match_details[names(match_details) != "mt_squad_one"]) %>%
      mutate(id = row_number(),
             mt_id = 0,
             mt_team = mt_team_two,
             mt_opposition = mt_team_one,
             pl_name = mt_squad_two,
             in_no = 2) %>%
      select(-mt_team_one, -mt_team_two, -mt_squad_two)
    
  )
  names(innings) <- c(match_details$mt_team_one, match_details$mt_team_two)
  
  innings <- innings %>%
    lapply(function(x){
      x %>% 
      mutate(in_bat = row_number()) %>%
      left_join(readRDS("data/database/player.rds") %>% select(pl_id, pl_name),
              by = "pl_name")})
    
    
  
  

# Pick teams
source("code/highest_batsman/01_highest_batsman_preparation.R")

teams <- innings %>%
  lapply(function(x){
    x %>% 
    left_join(highest_batsman_preparation(x %>% filter(!is.na(pl_id))), 
              by = "id") %>%
    mutate(games_played = ph_pf_runs_sum_career / ph_pf_runs_mean_career) %>%
    arrange(-games_played) %>%
    filter(row_number() <= 11) %>%
    mutate(in_bat = row_number(games_played)) %>%
    arrange(in_bat) %>%
    .[, names(innings[[1]])]
  })
  




# Add attrs
source("code/highest_batsman/01_highest_batsman_preparation.R")

teams <- teams %>%
  lapply(function(x){
    x %>% 
      left_join(highest_batsman_preparation(x %>% filter(!is.na(pl_id))), 
                by = "id")})


  
# Predict

model <- readRDS("data/highest_batsman/model.rds")
attrs <- readRDS("data/highest_batsman/attrs.rds")  

prediction <- teams %>%
  lapply(function(x){
    x %>%
    mutate(pr_highest_bat = predict(model, sapply(x[, attrs], as.numeric), 
                                  missing = NaN)) %>%
  group_by(mt_team) %>%
  mutate(pr_highest_bat_team = sum(pr_highest_bat)) %>%
  ungroup() %>%
  mutate(pr_highest_bat = pr_highest_bat / pr_highest_bat_team,
         pr_highest_bat_odds = 1 / pr_highest_bat) %>%
  arrange(pr_highest_bat_odds) %>%
  mutate(
    minus_10_perc = 0.9 * pr_highest_bat_odds,
    plus_10_perc = 1.1 * pr_highest_bat_odds) %>%
  select(mt_team, pl_name, pr_highest_bat, minus_10_perc, pr_highest_bat_odds, plus_10_perc)
  })


return(prediction)
  
}
  
  