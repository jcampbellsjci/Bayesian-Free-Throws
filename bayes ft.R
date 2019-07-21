#### Bayesian Free Throw Project ####


#### Libraries ####

library(nbastatR)
library(rjson)
library(png)
library(grid)
library(tidyverse)


#### Pre-set Up ####

# Loading up game logs from 2019
logs <- game_logs(seasons = 2019, result_types = "player", 
                  assign_to_environment = F, 
                  season_types = c("Regular Season", "Playoffs"))

# Loading up team dictionary
team_dictionary <- nba_teams() 


#### Function it Up ####

# date_ids pulls boxscore information from a given date
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

# ft_data calculates the posterior and gives a plot as well as text output
ft_data <- function(month, day, year) {
  
  # Using date_ids to get available games
  game_id_today <- date_ids(month = month, day = day, year = year)
  
  # We only have team ids, so we need to filter the team_dictionary to get 
  # names
  available_teams <- team_dictionary %>% 
    filter(idTeam %in% c(game_id_today$HOME_TEAM_ID, 
                         game_id_today$VISITOR_TEAM_ID)) 
  
  # User gets asked which of the available teams they'd want to choose
  team_choice <- readline(paste(available_teams$teamNameFull, 
                                collapse = "\n"))
  id_choice <- available_teams %>%
    filter(teamNameFull == team_choice)
  
  # Filtering the list of available games by the team the user chose
  game_id <- game_id_today %>%
    filter(HOME_TEAM_ID == id_choice$idTeam |
             VISITOR_TEAM_ID == id_choice$idTeam) %>%
    select(GAME_ID)
  
  # Pulling box score information from that game id
  current <- box_scores(game_ids = game_id$GAME_ID, 
                        box_score_types = c("Traditional"), 
                        result_types = c("player"), 
                        assign_to_environment = F) %>%
    .$dataBoxScore %>%
    .[[1]] %>%
    filter(idTeam == id_choice$idTeam) %>%
    select(namePlayer, ftm, fta)
  
  # Allowing the user to choose which player they want
  player_choice <- readline(paste(current$namePlayer, collapse = "\n"))
  
  # Getting the last five game logs for the given player
  # This is used as the prior
  game_logs <- logs %>%
    filter(namePlayer == player_choice) %>%
    mutate(dateGame = as.Date(dateGame)) %>%
    top_n(n = 5, dateGame) %>%
    select(namePlayer, ftm, fta, urlPlayerThumbnail) %>%
    group_by(namePlayer, urlPlayerThumbnail) %>%
    summarize(total_ftm = sum(ftm), total_fta = sum(fta)) %>%
    ungroup()
  
  # Filtering box score info for the chosen player
  final <- current %>%
    filter(namePlayer == player_choice) %>%
    left_join(game_logs)
  
  # Calculating summary statistics and posterior
  final_percent <- final %>%
    group_by(namePlayer) %>%
    summarize(this_game = ftm / fta, 
              past_five = total_ftm / total_fta, 
              posterior = (ftm + total_ftm) / (fta + total_fta)) %>%
    # Going to long format for plotting purposes
    gather(key = "Category", value = "Value", this_game:posterior) %>%
    mutate(Category = factor(Category, levels = c("this_game", "past_five",
                                                  "posterior")))
  # Doing another version for text output
  final_count <- final %>%
    group_by(namePlayer) %>%
    mutate(this_game = ftm / fta, 
           past_five = total_ftm / total_fta, 
           posterior = (ftm + total_ftm) / (fta + total_fta))
  
  # Pulling player thumbnails for plot
  myurl <- final$urlPlayerThumbnail
  z <- tempfile()
  download.file(myurl, z, mode="wb")
  pic <- readPNG(z)
  file.remove(z)
  pic <- rasterGrob(pic, interpolate = T)
  
  # Creating plot
  final_plot <- final_percent %>%
    mutate(Category = factor(Category, levels = c(levels(Category), ""))) %>%
    ggplot(aes(x = Category, y = Value)) +
    annotation_custom(pic, xmin = 3.5, xmax = 4.5, ymin = -.06, 
                      ymax = .3) +
    geom_segment(aes(x = Category, xend = Category, y = 0, yend = Value)) +
    geom_point(size = 5, fill = "white", pch = 22) +
    ylim(0, 1) +
    scale_x_discrete(drop = F, labels = c("Today", "Past Five", "Expected", "")) +
    labs(y = "Percent", title = "Free Throw Percentages")
  
  output <- list(final_plot, final_count)
  print(output)
}

a <- ft_data(4, 7, 2019)
