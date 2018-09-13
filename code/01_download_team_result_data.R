library(tidyverse)
library(rvest)

download_team_result_data <- function(){
  
  # Check if dataset exists
  if(file.exists("data/team_result.rds")){
    # Load batting history
    ds <- readRDS("data/team_result.rds")
    first_download <- 0
  } else{
    first_download <- 1
  }
  
  
  first_page <- ifelse(first_download == 1, 1,
                ifelse(first_download == 0 & max(ds$page) < 4, 1, max(ds$page) - 3))
  
  
  
  
  # Download each page needed
  add <- list()
  i <- first_page
  repeat {
    print(paste0("Downloading page ", i))
    url <- paste0("http://stats.espncricinfo.com/ci/engine/stats/index.html?class=11;page=", i, ";template=results;type=team;view=results")
    doc <- xml2::read_html(url)
    page <- rvest::html_table(x = rvest::html_nodes(doc, "table.engineTable")[[3]], header = T)
    
    if(nrow(page) > 0) {
    
      page$page <- i
      page$row <- 1:nrow(page)
      
      add[[i  - first_page + 1]] <- page
      i <- i + 1
      
    } else {break}
  }
  
  add <- lapply(add, function(x){x[, -c(7, 11)]})
  add <- bind_rows(add)
  add[] <- lapply(add, as.character)
  add <- add %>% mutate(page = as.integer(page),
                        row = as.integer(row))
  
  
  # Bind additional matches to beginning of history  
  if(first_download == 0) {ds <- bind_rows(add, ds)} else {ds <- add}
  
  # Dedup keeping latest version to ensure overwriting of history
  ds <- ds %>%
    distinct(page, row, .keep_all = T)
  
  
  # Save history
  saveRDS(ds, "data/team_result.rds")
  
  
}
