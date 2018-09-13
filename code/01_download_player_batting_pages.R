library(tidyverse)
library(rvest)

download_player_batting_pages <- function(){

# Check if batting history exists
if(file.exists("data/player_batting_pages.rds")){
  # Load batting history
  ds <- readRDS("data/player_batting_pages.rds")
  first_download <- 0
} else{
  first_download <- 1
  ds <- list()
}


first_page <- ifelse(first_download == 1, 1,
                ifelse(first_download == 0 & length(ds) < 4, 1, length(ds) - 3))

# Download each page needed
add <- list()
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
  
  page$page <- i
  page$row <- 1:nrow(page)
  
  add[[paste(i)]] <- page
  i <- i + 1
}


# Replace in original list
ds[names(add)] <- add[names(add)]


# Save history
saveRDS(ds, "data/player_batting_pages.rds")

}


