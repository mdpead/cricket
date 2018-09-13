
create_match_result_sample <- function(){

match <- readRDS("data/match.rds")
batting <- readRDS("data/batting.rds")

# Tidy
sample <- match %>% 
  distinct(in_start_date, in_ground, in_format) %>%
  select(in_start_date, in_ground, in_format, in_team, in_opposition, pf_result) %>%
  mutate(pf_result = ifelse(pf_result == "won", 1, 
                     ifelse(pf_result == "lost", 0, NA))) %>%
  filter(in_format %in% c("ODI", "T20I") & !is.na(pf_result)) %>%
  as.data.frame(.)

# Change variable formats
sample <- sample %>%
  mutate(in_ground = as.factor(in_ground),
         in_format = as.factor(in_format),
         in_team = as.factor(in_team),
         in_opposition = as.factor(in_opposition))


# Append Batting attributes

# Filter batting innings to those in match sample
batting <- batting %>% inner_join(sample %>% select(in_start_date, in_ground, in_format), by = c("in_start_date", "in_ground", "in_format"))

batting1 <- batting %>% filter(in_start_date < as.Date("1990-01-01"))
batting2 <- batting %>% filter(in_start_date >= as.Date("1990-01-01") & in_start_date < as.Date("2008-01-01"))
batting3 <- batting %>% filter(in_start_date >= as.Date("2008-01-01"))

batting1 <- add_batting_attributes(batting1)
batting2 <- add_batting_attributes(batting2)
batting3 <- add_batting_attributes(batting3)

batting <- rbind(batting1, batting2, batting3)
rm(batting1, batting2, batting3)

batting <- batting[, substring(names(batting), 1, 2) %in% c("in", "ph")]

# Roll up to match level
batting_agg <- batting %>%
  select(-in_inning, -in_bat, -in_player, -in_opposition, -in_country) %>%
  group_by(in_start_date, in_ground, in_format, in_team) %>%
  summarise_each(funs(sum(., na.rm = T)))

# Join on for team and opposition
sample <- sample %>%
  left_join(batting_agg %>% setNames(., paste0("team_", names(.))), 
            by = c("in_start_date" = "team_in_start_date", "in_team" = "team_in_team", "in_ground" = "team_in_ground", "in_format" = "team_in_format")) %>%
  left_join(batting_agg %>% setNames(., paste0("opposition_", names(.))),
            by = c("in_start_date" = "opposition_in_start_date", "in_opposition" = "opposition_in_team", "in_ground" = "opposition_in_ground", "in_format" = "opposition_in_format"))



# Add build and validation split
set.seed(0)
s <- sample(nrow(sample), nrow(sample) * 0.7)
sample$pr_pop <- ifelse(1:nrow(sample) %in% s, "build", "val")


# Replace NaNs with NA (figure out why later)
# sample[is.nan(as.matrix(sample))] <- NA



# Save
saveRDS(sample, "samples/match_result_sample.rds")

}
