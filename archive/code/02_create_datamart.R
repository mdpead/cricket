library(tidyverse)
library(stringr)

create_datamart <- function(){

  
########## Load downloads ##########

player_batting_dl <- readRDS("data/player_batting.rds")
# player_bowling_dl <- readRDS("data/player_bowling.rds")
team_results_dl <- readRDS("data/team_result.rds")
country_lookup <- read_csv("data/country_lookup.csv")




########## Dimensions ##########



##### Match #####

match <- team_results_dl %>%
  arrange(page, row) %>%
  transmute(
    mt_start_date = as.Date(`Start Date`, format = "%d %B %Y"),
    mt_ground = Ground,
    mt_format = substr(Opposition, 1, regexpr(pattern = " v ", Opposition) - 1),
    mt_team_one = Team,
    mt_team_two = substr(Opposition, regexpr(pattern = " v ", Opposition) + 3, nchar(Opposition))
  ) %>% 
  distinct(mt_start_date, mt_ground, mt_format, .keep_all = T) %>%
  mutate(mt_id = row_number())



##### Player #####

player <- player_batting_dl %>%
  arrange(page, row) %>%
  mutate(player = substr(Player, 1, regexpr(pattern = "\\(", Player) - 2)) %>%
  distinct(player, .keep_all = T) %>%
  transmute(pl_id = row_number(),
            pl_name = player
  )
  
  













########## Facts ##########



##### Match Result #####

match_result <- team_results_dl %>% 
  arrange(page, row) %>%
  transmute(
    mt_start_date = as.Date(`Start Date`, format = "%d %B %Y"),
    mt_ground = Ground,
    mt_format = substr(Opposition, 1, regexpr(pattern = " v ", Opposition) - 1),
    mt_team_one = Team,
    mt_team_two = substr(Opposition, regexpr(pattern = " v ", Opposition) + 3, nchar(Opposition)),
    re_team = ifelse(Result == "won", mt_team_one, ifelse(Result == "lost", mt_team_two, Result)),
    re_type = ifelse(str_extract(Margin, "[a-z]+") == "inns", "runs", str_extract(Margin, "[a-z]+")),
    re_inning_margin = ifelse(str_extract(Margin, "[a-z]+") == "inns", 1, 0),
    re_margin = str_extract(Margin, "[0-9]+"),
    re_balls_remaining = BR,
    re_toss = ifelse(Toss == "won", mt_team_one, mt_team_two),
    re_first_bat = ifelse(str_extract(Bat, "[0-9]") == 1, mt_team_one, mt_team_two)
  ) %>%
  distinct(mt_start_date, mt_ground, mt_format, .keep_all = T) %>%
  left_join(match, by = c("mt_start_date", "mt_ground", "mt_format")) %>%
  select(mt_id, starts_with("re_"))




##### Player Batting Innings #####

player_batting_inning <- player_batting_dl %>% 
  arrange(page, row) %>%
  transmute(
    id = row_number(),
    mt_start_date = as.Date(Start.Date, format = "%d %B %Y"),
    mt_ground = Ground,
    mt_format = substr(Opposition, 1, regexpr(pattern = " v ", Opposition) - 1),
    mt_team_code = toupper(substr(Player, regexpr(pattern = "\\(", Player) + 1, regexpr(pattern = "\\)", Player) - 1)),
    mt_opposition = substr(Opposition, regexpr(pattern = " v ", Opposition) + 3, nchar(Opposition)),
    in_no = as.integer(ifelse(is.na(Inns), 0, Inns)),
    pl_name = substr(Player, 1, regexpr(pattern = "\\(", Player) - 2),
    pf_runs = as.integer(ifelse(Runs == "DNB", 0, gsub("\\*", "", Runs))),
    pf_balls = as.integer(ifelse(Runs == "DNB", 0, BF)),
    pf_mins = as.integer(ifelse(Runs == "DNB" | Mins == "-", 0, Mins)),
    pf_fours = as.integer(ifelse(Runs == "DNB", 0, X4s)),
    pf_sixes = as.integer(ifelse(Runs == "DNB", 0, X6s)),
    pf_no = ifelse(substr(Runs, nchar(Runs), nchar(Runs)) == "*", 1, 0),
    pf_dnb = ifelse(Runs == "DNB", 1, 0)
  ) %>%
  left_join(country_lookup, by = c("mt_team_code" = "team_code")) %>%
  rename(mt_team = team_name) %>%
  select(-mt_team_code) %>%
  left_join(match %>% select(mt_id, mt_start_date, mt_ground, mt_format), by = c("mt_start_date", "mt_ground", "mt_format")) %>%
  left_join(player %>% select(pl_id, pl_name), by = c("pl_name")) %>%
  group_by(mt_id, in_no) %>%
  mutate(in_bat = row_number(),
         pf_rank_bat = as.numeric(ifelse(in_no != 0, min_rank(-pf_runs), NA)),
         pf_highest_bat = as.numeric(ifelse(pf_rank_bat == 1, 1, 0))) %>%
  ungroup()


##### Player Bowling Innings #####

# player_bowling_inning <- player_bowling_dl %>% 
#   arrange(page, row) %>% 
#   mutate(
#     start_date = as.Date(Start.Date, format = "%d %B %Y"),
#     ground = Ground,
#     format = substr(Opposition, 1, regexpr(pattern = " v ", Opposition) - 1),
#     team = toupper(substr(Player, regexpr(pattern = "\\(", Player) + 1, regexpr(pattern = "\\)", Player) - 1)),
#     opposition = substr(Opposition, regexpr(pattern = " v ", Opposition) + 3, nchar(Opposition)),
#     inning = as.integer(ifelse(is.na(Inns), 0, Inns)),
#     id = row_number(),
#     player = substr(Player, 1, regexpr(pattern = "\\(", Player) - 2),
#     balls = as.integer(ifelse(Overs == "DNB", 0,  
#       floor(as.numeric(Overs)) * as.numeric(BPO) + as.numeric(substr(Overs, nchar(Overs), nchar(Overs))))),
#     runs = as.integer(ifelse(Overs == "DNB", 0, Runs)),
#     wickets = as.integer(ifelse(Overs == "DNB", 0, Wkts)),
#     min_dot_balls = as.integer(ifelse(Overs == "DNB", 0, as.numeric(Mdns) * as.numeric(BPO))),
#     inning = as.integer(Inns)
#   ) %>%
#   left_join(match %>% select(start_date, ground, format, id) %>% rename(match_id = id), 
#             by = c("start_date", "ground", "format")) %>%
#   left_join(ground_country_lookup, by = c("ground" = "ground")) %>%
#   group_by(match_id, inning) %>%
#   mutate(bowl = ifelse(balls > 0, row_number(), NA)) %>%
#   ungroup() %>%
#   select(id, match_id, start_date, ground, country, format, team, opposition, inning, bowl, player, balls, runs, wickets, min_dot_balls)




output <- list(
  match = match,
  player = player,
  match_result = match_result,
  player_batting_inning = player_batting_inning
)


return(output)



}
