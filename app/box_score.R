box_score <- function(date, game_id, team_id){
  combo_date <- gsub("-", "", date)
  
  url <- paste("https://data.nba.net/prod/v1/",
               combo_date, "/", game_id, "_boxscore.json",
               sep = "")
  
  raw_data <- rjson::fromJSON(file = url)
  
  stats <- tibble(teamID = sapply(raw_data$stats$activePlayers, "[[", 5),
                  namePlayer = paste(sapply(raw_data$stats$activePlayers, "[[", 2),
                                     sapply(raw_data$stats$activePlayers, "[[", 3)),
                  fgm = sapply(raw_data$stats$activePlayers, "[[", 12),
                  fga = sapply(raw_data$stats$activePlayers, "[[", 13),
                  fg3m = sapply(raw_data$stats$activePlayers, "[[", 18),
                  fg3a = sapply(raw_data$stats$activePlayers, "[[", 19),
                  ftm = sapply(raw_data$stats$activePlayers, "[[", 15),
                  fta = sapply(raw_data$stats$activePlayers, "[[", 16)) %>%
    filter(teamID == team_id) %>%
    mutate_at(.vars = vars(fgm:fta), .funs = funs(as.numeric))
  
  stats
}
