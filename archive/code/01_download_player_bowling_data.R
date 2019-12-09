library(dplyr)

download_player_bowling_data <- function(){
  
  # Check if batting history exists
  if(file.exists("data/downloads/player_bowling.rds")){
    # Load batting history
    ds <- readRDS("data/downloads/player_bowling.rds")
    first_download <- 0
  } else{
    first_download <- 1
  }
  
  
  first_page <- ifelse(first_download == 1, 1,
                       ifelse(first_download == 0 & max(ds$page) < 4, 1, max(ds$page) - 3))
  
  # Download each page needed
  i <- first_page
  repeat {
    print(paste("Downloading page ", i, sep = ""))
    url <- paste("http://stats.espncricinfo.com/ci/engine/stats/index.html?class=11;orderby=start;page=", i, ";template=results;type=bowling;view=innings;wrappertype=print", sep = "")
    doc <- xml2::read_html(url)
    page <- rvest::html_table(x = rvest::html_nodes(doc, "table.engineTable")[[3]], header = T)
    
    page <- data.frame(lapply(page, as.character), stringsAsFactors = FALSE)
    page$page <- i
    page$row <- 1:nrow(page)
    
    if(nrow(page) > 0) {i <- i + 1} else {break}
    if(i != first_page + 1) {add <- bind_rows(add, page)} else {add <- page}
  }
  
  add <- add[, -9]
  
  
  # Bind additional innings to beginning of history  
  if(first_download == 0) {ds <- bind_rows(add, ds)} else {ds <- add}
  
  # Dedup keeping latest version to ensure overwriting of history
  ds <- ds %>%
    distinct(page, row, .keep_all = T)
  
  
  # Save history
  saveRDS(ds, "data/downloads/player_bowling.rds")
  
}


