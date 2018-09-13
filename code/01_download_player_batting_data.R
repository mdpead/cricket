library(tidyverse)
library(rvest)

download_player_batting_data <- function(){

# Check if batting history exists
if(file.exists("data/player_batting.rds")){
  # Load batting history
  ds <- readRDS("data/player_batting.rds")
  first_download <- 0
} else{
  first_download <- 1
}


first_page <- ifelse(first_download == 1, 1,
                ifelse(first_download == 0 & max(ds$page) < 4, 1, max(ds$page) - 3))

# Download each page needed
add <- NULL
page <- NULL
i <- first_page
repeat {
  print(paste("Downloading page ", i, sep = ""))
  url <- paste("http://stats.espncricinfo.com/ci/engine/stats/index.html?class=11;orderby=start;page=", i, ";template=results;type=batting;view=innings;wrappertype=print", sep = "")
  
  page <- tryCatch(read_html(url), error = function(c) break)
    
  page <- page %>%
    html_nodes("table.engineTable") %>% 
    .[[3]] %>%
    html_table(header = T)
  
  
  if(!nrow(page) > 0) {break}
  page <- data.frame(lapply(page, as.character), stringsAsFactors = FALSE)
  page$page <- i
  page$row <- 1:nrow(page)
  
  
  if(i != first_page) {add <- bind_rows(add, page)} else {add <- page}
  i <- i + 1
}

add <- add[, -9]


# Bind additional innings to beginning of history  
if(first_download == 0) {ds <- bind_rows(add, ds)} else {ds <- add}

# Dedup keeping latest version to ensure overwriting of history
ds <- ds %>%
  distinct(page, row, .keep_all = T)


# Save history
saveRDS(ds, "data/player_batting.rds")

}


