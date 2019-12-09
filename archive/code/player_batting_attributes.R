library(tidyverse)


player_batting_attributes <- function(){

  
##### Load data #####

data <- create_datamart()


##### Filter #####

history <- data$player_batting_inning

# Filter to opening innings to reduce noise from test matches (possibly remove later)
history <- history %>%
  filter(in_no %in% c(1,2)) %>%
  filter(!is.na(mt_id))

##### Attributes #####

attrs <- history %>%
  arrange(pl_id, mt_start_date) %>%
  group_by(pl_id) %>%
  mutate(
    pb_runs_career = cumsum(pf_runs),
    pb_balls_career = 
  )
  



return(attrs)
}
