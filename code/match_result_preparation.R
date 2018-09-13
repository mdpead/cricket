
team_attrs <- function(input){
  
##### Check input #####
reqs <- c("in_start_date", "in_format", "in_ground", "in_team", "in_opposition", "in_player", "in_bat")
checks <- sapply(reqs, function(x){x %in% names(input)})

for (i in 1:length(reqs)){
  if(checks[i] == F)(stop(paste0("Input missing column ", reqs[i])))
}
  
  
###### Add batting attributes #######
  
# Add player batting attributes
batting <- add_player_batting_attrs(input)
  
  
# Roll up to match level
batting_agg <- batting %>%
  select(-in_bat, -in_player, -in_opposition, -in_country) %>%
  group_by(in_start_date, in_ground, in_format, in_team) %>%
  summarise_each(funs(sum(., na.rm = T)))
  
# Join on for team and opposition
attrs <- input %>%
  select(in_start_date, in_ground, in_format, in_team, in_opposition) %>%
  distinct(in_start_date, in_ground, in_format, in_team, in_opposition) %>%
  left_join(batting_agg %>% setNames(., paste0("team_", names(.))), 
            by = c("in_start_date" = "team_in_start_date", "in_team" = "team_in_team", "in_ground" = "team_in_ground", "in_format" = "team_in_format")) %>%
  left_join(batting_agg %>% setNames(., paste0("opposition_", names(.))),
            by = c("in_start_date" = "opposition_in_start_date", "in_opposition" = "opposition_in_team", "in_ground" = "opposition_in_ground", "in_format" = "opposition_in_format"))

  
# Replace NAs with 0s where applicable
attrs[, substring(names(attrs), 1, 2) == "ph" & substring(names(attrs), 4, 6) != "avg"][is.na(attrs[, substring(names(attrs), 1, 2) == "ph" & substring(names(attrs), 4, 6) != "avg"])] <- 0

# Ensure dataframe returned
attrs <- as.data.frame(attrs)

return(attrs)
}
