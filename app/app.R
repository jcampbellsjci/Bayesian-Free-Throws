#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(scales)
library(nbastatR)
library(rjson)
library(httr)
library(tidyverse)

# Sourcing the date_ids function
source("date_ids.R")
source("box_score.R")

# Doing an initial load team dictionary and game logs
team_dictionary <- nba_teams()
logs <- game_logs(seasons = 2020, result_types = "player",
                  assign_to_environment = F,
                  season_types = c("Regular Season"))

# Define UI for application that draws a histogram
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
                value = Sys.Date()),
      
      uiOutput("team_selection"),
      
      uiOutput("player_selection"),
      
      sliderInput("prior",
                  "Prior",
                  min = 1, max = 10, value = 1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      tabPanel("Field Goals", tableOutput("table_fg"), plotOutput("plot_fg")),
      tabPanel("Three Pointers", tableOutput("table_3fg"),
               plotOutput("plot_3fg")),
      tabPanel("Free Throws", tableOutput("table_ft"), plotOutput("plot_ft"))))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  get_available_teams <- reactive({
    game_id_today <- date_ids(as.character(input$date))
    
    available_teams <- team_dictionary %>% 
      filter(idTeam %in% c(game_id_today$HOME_TEAM_ID, 
                           game_id_today$VISITOR_TEAM_ID))
    
    output <- list(game_id_today, available_teams)
  })
  
  # First output is the ui for the team selection input
  output$team_selection <- renderUI({
    available_teams <- get_available_teams()[[2]]
    
    selectInput("team", "Team", available_teams$teamNameFull)
  })
  
  get_box_score <- reactive({
    game_id_today <- get_available_teams()[[1]]
    available_teams <- get_available_teams()[[2]]
    
    id_choice <- available_teams %>%
      filter(teamNameFull == input$team)
    
    game_id <- game_id_today %>%
      filter(HOME_TEAM_ID == id_choice$idTeam |
               VISITOR_TEAM_ID == id_choice$idTeam) %>%
      select(GAME_ID)
    
    current <- box_score(date = input$date, game_id = game_id$GAME_ID,
                         team_id = id_choice$idTeam)
    if(nrow(current) == 0){
      current <- "No data yet"
    }
    
    output <- list(current)
  })
  
  # Next output is the ui for the player selection input
  output$player_selection <- renderUI({
    current <- get_box_score()[[1]]
    
    if(current == "No data yet"){
      selectInput("player", "Player", current)
    } else{
      selectInput("player", "Player", current$namePlayer)
    }
  })
  
  create_table <- reactive({
    current <- get_box_score()[[1]]
    
    if(current == "No data yet"){
      final <- current
      final_count <- current
    } else{
      game_logs <- logs %>%
        filter(namePlayer == input$player) %>%
        mutate(dateGame = as.Date(dateGame)) %>%
        filter(dateGame < input$date) %>%
        top_n(n = as.numeric(input$prior), dateGame) %>%
        select(namePlayer, fgm, fga, fg3m, fg3a, ftm, fta, 
               urlPlayerThumbnail) %>%
        group_by(namePlayer, urlPlayerThumbnail) %>%
        summarize(total_fgm = sum(fgm), total_fga = sum(fga),
                  total_fg3m = sum(fg3m), total_fg3a = sum(fg3a),
                  total_ftm = sum(ftm), total_fta = sum(fta)) %>%
        ungroup()
      
      final <- current %>%
        filter(namePlayer == input$player) %>%
        left_join(game_logs)
      
      final_count <- final %>%
        group_by(namePlayer) %>%
        mutate(this_game_fg = fgm / fga,
               prior_fg = total_fgm / total_fga,
               posterior_fg = (fgm + total_fgm) / (fga + total_fga),
               this_game_3fg = fg3m / fg3a,
               prior_3fg = total_fg3m / total_fg3a,
               posterior_3fg = (fg3m + total_fg3m) / (fg3a + total_fg3a),
               this_game_ft = ftm / fta, 
               prior_ft = total_ftm / total_fta, 
               posterior_ft = (ftm + total_ftm) / (fta + total_fta)) %>%
        mutate_at(.vars = vars(prior_fg, posterior_fg),
                  .funs = funs(ifelse(is.na(.) == T, this_game_fg, .))) %>%
        mutate_at(.vars = vars(prior_3fg, posterior_3fg),
                  .funs = funs(ifelse(is.na(.) == T, this_game_3fg, .))) %>%
        mutate_at(.vars = vars(prior_ft, posterior_ft),
                  .funs = funs(ifelse(is.na(.) == T, this_game_ft, .))) %>%
        select(- urlPlayerThumbnail)
    }
    
    output <- list(final, final_count)
  })
  
  # Next we create the table that will be in the main panel
  output$table_fg <- renderTable({
    final_count <- create_table()[[2]]
    
    if(final_count == "No data yet"){
      final_count
    } else{
      final_count <- final_count %>%
        select(namePlayer, fgm, fga, total_fgm, total_fga,
               this_game_fg, prior_fg, posterior_fg) %>%
        rename(Player = namePlayer, FGM_Today = fgm, FGA_Today = fga,
               FGM_Prior = total_fgm, FGA_Prior = total_fga,
               `FG%_Today` = this_game_fg, `FG%_Prior` = prior_fg,
               `FG%_Posterior` = posterior_fg)
    }
  })
  
  # Now creating the plot that will be used in the main panel
  output$plot_fg <- renderPlot({
    final <- create_table()[[1]]
    final_count <- create_table()[[2]]
    
    if(final_count == "No data yet"){
      final_count
    } else{
      final_percent <- final %>%
        group_by(namePlayer) %>%
        summarize(this_game_fg = fgm / fga, 
                  past_five_fg = total_fgm / total_fga,
                  posterior_fg = (fgm + total_fgm) / (fga + total_fga)) %>%
        gather(key = "Category", value = "Value", this_game_fg:posterior_fg) %>%
        mutate(Category = factor(Category, levels = c("this_game_fg",
                                                      "past_five_fg",
                                                      "posterior_fg")))
      
      final_percent %>%
        ggplot(aes(x = Category, y = Value)) +
        geom_segment(aes(x = Category, xend = Category, y = 0, yend = Value)) +
        geom_point(size = 5, fill = "white", pch = 22) +
        scale_y_continuous(labels = percent, limits = c(0, 1)) +
        scale_x_discrete(drop = F, labels = c("Today", "Prior", "Expected", "")) +
        labs(y = "Percent", title = "Field Goal Percentages")
    }
  })
  
  # Next we create the table that will be in the main panel
  output$table_3fg <- renderTable({
    final_count <- create_table()[[2]]
    
    if(final_count == "No data yet"){
      final_count
    } else{
      final_count <- final_count %>%
        select(namePlayer, contains("3")) %>%
        rename(Player = namePlayer, FG3M_Today = fg3m, FG3A_Today = fg3a,
               FG3M_Prior = total_fg3m, FG3A_Prior = total_fg3a,
               `FG3%_Today` = this_game_3fg, `FG3%_Prior` = prior_3fg,
               `FG3%_Posterior` = posterior_3fg)
    }
  })
  
  # Now creating the plot that will be used in the main panel
  output$plot_3fg <- renderPlot({
    final <- create_table()[[1]]
    final_count <- create_table()[[2]]
    
    if(final_count == "No data yet"){
      final_count
    } else{
      final_percent <- final %>%
        group_by(namePlayer) %>%
        summarize(this_game_3fg = fg3m / fg3a, 
                  past_five_3fg = total_fg3m / total_fg3a,
                  posterior_3fg = (fg3m + total_fg3m) / (fg3a + total_fg3a)) %>%
        gather(key = "Category", value = "Value", 
               this_game_3fg:posterior_3fg) %>%
        mutate(Category = factor(Category, levels = c("this_game_3fg",
                                                      "past_five_3fg",
                                                      "posterior_3fg")))
      
      final_percent %>%
        ggplot(aes(x = Category, y = Value)) +
        geom_segment(aes(x = Category, xend = Category, y = 0, yend = Value)) +
        geom_point(size = 5, fill = "white", pch = 22) +
        scale_y_continuous(labels = percent, limits = c(0, 1)) +
        scale_x_discrete(drop = F, 
                         labels = c("Today", "Prior", "Expected", "")) +
        labs(y = "Percent", title = "Three Point Percentages")
    }
  })
  
  # Next we create the table that will be in the main panel
  output$table_ft <- renderTable({
    final_count <- create_table()[[2]]
    
    if(final_count == "No data yet"){
      final_count
    } else{
      final_count <- final_count %>%
        select(namePlayer, contains("ft")) %>%
        rename(Player = namePlayer, FTM_Today = ftm, FTA_Today = fta,
               FTM_Prior = total_ftm, FTA_Prior = total_fta,
               `FT%_Today` = this_game_ft, `FT%_Prior` = prior_ft,
               `FT%_Posterior` = posterior_ft)
    }
  })
  
  # Now creating the plot that will be used in the main panel
  output$plot_ft <- renderPlot({
    final <- create_table()[[1]]
    final_count <- create_table()[[2]]
    
    if(final_count == "No data yet"){
      final_count
    } else{
      final_percent <- final %>%
        group_by(namePlayer) %>%
        summarize(this_game_ft = ftm / fta, 
                  prior_ft = total_ftm / total_fta, 
                  posterior_ft = (ftm + total_ftm) / (fta + total_fta)) %>%
        gather(key = "Category", value = "Value", this_game_ft:posterior_ft) %>%
        mutate(Category = factor(Category, levels = c("this_game_ft",
                                                      "prior_ft",
                                                      "posterior_ft")))
      
      final_percent %>%
        ggplot(aes(x = Category, y = Value)) +
        geom_segment(aes(x = Category, xend = Category, y = 0, yend = Value)) +
        geom_point(size = 5, fill = "white", pch = 22) +
        scale_y_continuous(labels = percent, limits = c(0, 1)) +
        scale_x_discrete(drop = F,
                         labels = c("Today", "Prior", "Expected", "")) +
        labs(y = "Percent", title = "Free Throw Percentages")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)