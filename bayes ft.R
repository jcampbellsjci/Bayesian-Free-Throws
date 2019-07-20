library(nbastatR)
library(rjson)
library(tidyverse)

logs <- game_logs(seasons = 2019, result_types = "player", 
                  assign_to_environment = F, 
                  season_types = c("Regular Season", "Playoffs"))
team_dictionary <- nba_teams() 

date_ids <- function(month, day, year) {
  url <- paste("https://stats.nba.com/stats/scoreboardV2?DayOffset=0&LeagueID=00&gameDate=", 
               month, "%2F", day, "%2F", year, sep = "")
  
  data_json <- fromJSON(file = url)
  
  data_df <- map_df(1:length(data_json$resultSets[[1]]$rowSet), 
                    ~ bind_cols(
                      data_json$resultSets[[1]]$rowSet[[.x]][1:8]))
  
  colnames(data_df) <- data_json$resultSets[[1]]$headers[1:8]
  
  data_df
}

ft_data <- function(month, day, year) {
  game_id_today <- date_ids(month = month, day = day, year = year)
  
  
  available_teams <- team_dictionary %>% 
    filter(idTeam %in% c(game_id_today$HOME_TEAM_ID, 
                         game_id_today$VISITOR_TEAM_ID)) 
  
  team_choice <- readline(paste(available_teams$teamNameFull, 
                                collapse = "\n"))
  id_choice <- available_teams %>%
    filter(teamNameFull == team_choice)
  
  game_id <- game_id_today %>%
    filter(HOME_TEAM_ID == id_choice$idTeam |
             VISITOR_TEAM_ID == id_choice$idTeam) %>%
    select(GAME_ID)
  
  current <- box_scores(game_ids = game_id$GAME_ID, 
                        box_score_types = c("Traditional"), 
                        result_types = c("player"), 
                        assign_to_environment = F) %>%
    .$dataBoxScore %>%
    .[[1]] %>%
    filter(idTeam == id_choice$idTeam) %>%
    select(namePlayer, ftm, fta)
  
  player_choice <- readline(paste(current$namePlayer, collapse = "\n"))
  
  game_logs <- logs %>%
    filter(namePlayer == player_choice) %>%
    mutate(dateGame = as.Date(dateGame)) %>%
    top_n(n = 5, dateGame) %>%
    select(namePlayer, ftm, fta, urlPlayerThumbnail) %>%
    group_by(namePlayer, urlPlayerThumbnail) %>%
    summarize(total_ftm = sum(ftm), total_fta = sum(fta)) %>%
    ungroup()
  
  final <- current %>%
    filter(namePlayer == player_choice) %>%
    left_join(game_logs)
  
  final_percent <- final %>%
    group_by(namePlayer) %>%
    summarize(this_game = ftm / fta, 
              past_five = total_ftm / total_fta, 
              posterior = (ftm + total_ftm) / (fta + total_fta)) %>%
    gather(key = "Category", value = "Value", this_game:posterior) %>%
    mutate(Category = factor(Category, levels = c("this_game", "past_five",
                                                  "posterior")))
  
  final_count <- final %>%
    group_by(namePlayer) %>%
    mutate(this_game = ftm / fta, 
           past_five = total_ftm / total_fta, 
           posterior = (ftm + total_ftm) / (fta + total_fta))
  
  myurl <- final$urlPlayerThumbnail
  z <- tempfile()
  download.file(myurl, z, mode="wb")
  pic <- readPNG(z)
  file.remove(z)
  pic <- rasterGrob(pic, interpolate = T)
  
  final_plot <- final_percent %>%
    ggplot(aes(x = Category, y = Value)) +
    annotation_custom(pic, xmin=2.75, xmax=3.25, ymin=max(final_percent$Value, na.rm = T), 
                      ymax=max(final_percent$Value, na.rm = T)*1.25) +
    geom_col() + 
    ylim(0, max(final_percent$Value, na.rm = T)*1.25)
  
  
  print(final_plot)
  print(final_count)
}

a <- ft_data(4, 7, 2019)
