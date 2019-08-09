library(shiny)
library(rjson)
library(nbastatR)
library(png)
library(grid)
library(scales)
library(tidyverse)

# Sourcing the date_ids function
source("date_ids.R")

# Doing an initial load team dictionary and game logs
team_dictionary <- nba_teams()
logs <- game_logs(seasons = 2019, result_types = "player", 
                  assign_to_environment = F, 
                  season_types = c("Regular Season", "Playoffs"))

ui <- fluidPage(
  
  titlePanel("Bayes Free Throw"),
  
  sidebarLayout(
    # Sidebar has a date input, and a ui output for teams and players
    # That way, teams available to choose are based on the date and players on
    # the team
    # Also have an input for prior amount of games
    sidebarPanel(
      dateInput("date",
                "Date",
                value = "2019-01-01"),
      
      uiOutput("team_selection"),
      
      uiOutput("player_selection"),
      
      selectInput("prior",
                  "Prior",
                  choices = c(3, 5, 10))
    ),
    
    # Main Panel plots the graph and produces a table of info
    mainPanel(plotOutput("plot"),
              tableOutput("table"))
  )
  
)

server <- function(input, output) {
  
  # First output is the ui for the team selection input
  output$team_selection <- renderUI({
    
    game_id_today <- date_ids(as.character(input$date))
    
    # Filter out any team that doesn't play on a given date
    available_teams <- team_dictionary %>% 
      filter(idTeam %in% c(game_id_today$HOME_TEAM_ID, 
                           game_id_today$VISITOR_TEAM_ID))
    
    selectInput("team", "Team", available_teams$teamNameFull)
    
  })
  
  # Next output is the ui for the player selection input
  output$player_selection <- renderUI({
    
    game_id_today <- date_ids(as.character(input$date))
    
    available_teams <- team_dictionary %>% 
      filter(idTeam %in% c(game_id_today$HOME_TEAM_ID, 
                           game_id_today$VISITOR_TEAM_ID))
    
    id_choice <- available_teams %>%
      filter(teamNameFull == input$team)
    
    # Selecting game id for the chosen team
    game_id <- game_id_today %>%
      filter(HOME_TEAM_ID == id_choice$idTeam |
               VISITOR_TEAM_ID == id_choice$idTeam) %>%
      select(GAME_ID)
    
    # Using the game id to get the box score
    current <- box_scores(game_ids = game_id$GAME_ID, 
                          box_score_types = c("Traditional"), 
                          result_types = c("player"), 
                          assign_to_environment = F) %>%
      .$dataBoxScore %>%
      .[[1]] %>%
      filter(idTeam == id_choice$idTeam) %>%
      select(namePlayer, ftm, fta)
    
    selectInput("player", "Player", current$namePlayer)
    
  })
  
  # Next we create the table that will be in the main panel
  output$table <- renderTable({
    
    game_id_today <- date_ids(as.character(input$date))
    
    available_teams <- team_dictionary %>% 
      filter(idTeam %in% c(game_id_today$HOME_TEAM_ID, 
                           game_id_today$VISITOR_TEAM_ID))
    
    id_choice <- available_teams %>%
      filter(teamNameFull == input$team)
    
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
    
    game_logs <- logs %>%
      filter(namePlayer == input$player) %>%
      mutate(dateGame = as.Date(dateGame)) %>%
      top_n(n = as.numeric(input$prior), dateGame) %>%
      select(namePlayer, ftm, fta, urlPlayerThumbnail) %>%
      group_by(namePlayer, urlPlayerThumbnail) %>%
      summarize(total_ftm = sum(ftm), total_fta = sum(fta)) %>%
      ungroup()
    
    final <- current %>%
      filter(namePlayer == input$player) %>%
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
             posterior = (ftm + total_ftm) / (fta + total_fta)) %>%
      select(- urlPlayerThumbnail)
    
    final_count
  })
  
  # Now creating the plot that will be used in the main panel
  output$plot <- renderPlot({
    
    game_id_today <- date_ids(as.character(input$date))
    
    available_teams <- team_dictionary %>% 
      filter(idTeam %in% c(game_id_today$HOME_TEAM_ID, 
                           game_id_today$VISITOR_TEAM_ID))
    
    id_choice <- available_teams %>%
      filter(teamNameFull == input$team)
    
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
    
    game_logs <- logs %>%
      filter(namePlayer == input$player) %>%
      mutate(dateGame = as.Date(dateGame)) %>%
      top_n(n = as.numeric(input$prior), dateGame) %>%
      select(namePlayer, ftm, fta, urlPlayerThumbnail) %>%
      group_by(namePlayer, urlPlayerThumbnail) %>%
      summarize(total_ftm = sum(ftm), total_fta = sum(fta)) %>%
      ungroup()
    
    final <- current %>%
      filter(namePlayer == input$player) %>%
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
    
    final_percent %>%
      mutate(Category = factor(Category, levels = c(levels(Category), ""))) %>%
      ggplot(aes(x = Category, y = Value)) +
      annotation_custom(pic, xmin = 3.5, xmax = 4.5, ymin = -.06, 
                        ymax = .3) +
      geom_segment(aes(x = Category, xend = Category, y = 0, yend = Value)) +
      geom_point(size = 5, fill = "white", pch = 22) +
      scale_y_continuous(labels = percent, limits = c(0, 1)) +
      scale_x_discrete(drop = F, labels = c("Today", "Prior", "Expected", "")) +
      labs(y = "Percent", title = "Free Throw Percentages")
  })
  
  
}

shinyApp(ui = ui, server = server)