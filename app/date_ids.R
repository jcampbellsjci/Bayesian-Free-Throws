date_ids <- function(date) {
  month <- str_sub(date, 1, 4)
  day <- str_sub(date, 6, 7)
  year <- str_sub(date, 9, 10)
  
  url <- paste("https://stats.nba.com/stats/scoreboardV2?DayOffset=0&LeagueID=00&gameDate=", 
               month, "%2F", day, "%2F", year, sep = "")
  
  data_json <- fromJSON(file = url)
  
  data_df <- map_df(1:length(data_json$resultSets[[1]]$rowSet), 
                    ~ bind_cols(
                      data_json$resultSets[[1]]$rowSet[[.x]][1:8]))
  
  colnames(data_df) <- data_json$resultSets[[1]]$headers[1:8]
  
  data_df
  
}