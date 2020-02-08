date_ids <- function(date) {
  month <- str_sub(date, 1, 4)
  day <- str_sub(date, 6, 7)
  year <- str_sub(date, 9, 10)
  
  url <- paste("https://stats.nba.com/stats/scoreboardV2?DayOffset=0&LeagueID=00&gameDate=", 
               month, "%2F", day, "%2F", year, sep = "")
  
  headers = c(
    `Connection` = 'keep-alive',
    `Accept` = 'application/json, text/plain, */*',
    `x-nba-stats-token` = 'true',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36',
    `x-nba-stats-origin` = 'stats',
    `Sec-Fetch-Site` = 'same-origin',
    `Sec-Fetch-Mode` = 'cors',
    `Referer` = 'http://stats.nba.com/%referer%/',
    `Accept-Encoding` = 'gzip, deflate, br',
    `Accept-Language` = 'en-US,en;q=0.9'
  )
  
  res <- GET(url, add_headers(.headers = headers))
  data_json <- res$content %>%
    rawToChar() %>%
    fromJSON()
  
  data_df <- map_df(1:length(data_json$resultSets[[1]]$rowSet), 
                    ~ bind_cols(
                      data_json$resultSets[[1]]$rowSet[[.x]][1:8]))
  
  colnames(data_df) <- data_json$resultSets[[1]]$headers[1:8]
  
  data_df
  
}