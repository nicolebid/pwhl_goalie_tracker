options(shiny.port = 8050, shiny.autoreload = TRUE)
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(ggrepel)
library(DT)

team_ranking <- read_csv("../data/team_ranking.csv") 
goalie_wl <- read_csv("../data/goalie_wl.csv")
save_goals <- read_csv("../data/save_goals.csv")

teams <- c("Boston", "Minnesota", "Montreal", "New York",  "Ottawa", "Toronto")
gwl_metrics <- c("wins", "losses", "win to loss differential", "proportion of wins")


# UI
ui <- fluidPage(
  
  # Header
  fluidRow(
    column(
      width = 12,
      align = "center",
      h1("PWHL Goalie Tracker", style = "color: white; font-size: 30px; 
         background-color: darkslateblue; padding: 12px;")
    )
  ),
  
  # Main layout with two columns
  fluidRow(
    
    # COLUMN 1
    column(
      width = 4,
      
      # team selection
      div(
        style = "background-color: #f2f2f2; padding: 10px; margin-bottom: 15px;",
        selectizeInput(
          "team_select",
          "Select multiple teams:",
          choices = teams,
          multi = TRUE,
          options = list(
            placeholder = 'teams...',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
        
      ), 

      
      # table
      DTOutput("sp_table"), 
      textOutput("additional_text"),
      br(), 
      textOutput("selected_goalie_summary")
      
      
    ),
    
    # COLUMN 2 (PLOTS)
    column(
      width = 8,
      # Team Ranking plot
      div(
        style = "background-color: #f2f2f2; padding: 10px; margin-bottom: 15px;",
        plotOutput("team_plot", height = "325px")
      ),
      # GWL plot and order select
      div(
        style = "background-color: #f2f2f2; padding: 10px;",
        plotOutput("gwl_plot", height = "325px"),
        selectizeInput(
          "gwl_select",
          " ", 
          choices = gwl_metrics,
          multi = FALSE,
          selected = "wins",
          options = list(
            placeholder = 'Sort by...',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      )
    )
  )
)



# Server: Callbacks/reactivity
server <- function(input, output, session) {
  
  # TEAM RANKING PLOT ----------------------------------------------------- RANK
  output$team_plot <- renderPlot({
    team_colors <- c("New York" = rgb(0, 170, 170, maxColorValue = 255), 
                     "Boston" = "darkgreen", 
                     "Toronto" = rgb(100, 149, 237, maxColorValue = 255),
                     "Montreal" = "darkred", 
                     "Ottawa" = "red", 
                     "Minnesota" = "darkblue")
    
    # filter data for desired subset
    filtered_data <- subset(team_ranking, team %in% input$team_select)

    # Check if any teams are selected (default: plot all teams)
    if (length(input$team_select) == 0) {
      
      # for annotations 
      last_data <- team_ranking |> 
        group_by(team) |> 
        summarize(last_game_date = max(game_date),
                  last_total_points = max(total_points))
      
      # main plot 
      ggplot(team_ranking) +
        geom_line(aes(x=game_date, y=total_points, color=team)) + 
        scale_color_manual(values = team_colors) + 
        geom_text_repel(data = last_data, 
                        aes(x = last_game_date, 
                            y = last_total_points, label = team), 
                        nudge_x = 10, nudge_y = 0, direction = "y" , 
                        segment.alpha = 0.2) +
        geom_point(data = last_data, 
                   aes(x = last_game_date, y = last_total_points, color = team), 
                   size = 2) +
        xlim(as.Date('2024-01-01'), as.Date('2024-03-25')) + 
        labs(title = "Team Overall Standings", 
             y = "Cumulated Points", 
             x = "Time", 
             color = "Team") + 
        theme_minimal() + 
        theme(axis.line = element_line(color = "black"), 
              legend.position = "None", 
              plot.title = element_text(face = "bold", size = 14), 
              axis.text = element_text(size = 14), 
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14)) + 
        scale_x_date(date_labels = "%b")
      
      
    } else {
      
      # teams in filtered
      unique_teams <- unique(filtered_data$team)
      
      # colors of filtered teams
      team_colors_subset <- team_colors[unique_teams]
      
      # order data by ranking
      ordered_data <- filtered_data %>%
        arrange(desc(total_points))
      
      # for annotations 
      last_data <- ordered_data |> 
        group_by(team) |> 
        summarize(last_game_date = max(game_date),
                  last_total_points = max(total_points))
       
      # main plot
      ordered_data |> 
        ggplot() +
        geom_line(aes(x=game_date, y=total_points, color=team)) + 
        scale_color_manual(values = team_colors_subset) +
        geom_text_repel(data = last_data, 
                        aes(x = last_game_date, 
                            y = last_total_points, label = team), 
                        nudge_x = 10, nudge_y = 0, direction = "y" , 
                        segment.alpha = 0.2) +
        geom_point(data = last_data, 
                   aes(x = last_game_date, y = last_total_points, 
                       color = team), 
                   size = 2) +
        xlim(as.Date('2024-01-01'), as.Date('2024-03-25')) + 
        labs(title = "Team Overall Standings", 
             y = "Cumulated Points", 
             x = "Time", 
             color = "Team") + 
        theme_minimal() + 
        theme(axis.line = element_line(color = "black"), 
              legend.position = "None", 
              plot.title = element_text(face = "bold", size = 14), 
              axis.text = element_text(size = 14), 
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14)) + 
        scale_x_date(date_labels = "%b")
    }
  })
  
  # GOALIE WIN/LOSS PLOT ----------------------------------------------------GWL
  output$gwl_plot <- renderPlot({ 
    
    # check if teams are selected (Default: plot all teams)
    if (length(input$team_select) == 0) {
      
      order_select <- input$gwl_select
      goalie_wl_filter <- goalie_wl
      
      # Set Ordering
      if (order_select == "wins") {
        
        order <- goalie_wl_filter |> filter(outcome == "wins") |> 
          arrange(count)
        order <- order$player_name
        label_m <- "count"
        
      } else if (order_select == "losses") {
        order <- goalie_wl_filter |> filter(outcome == "losses") |> 
          arrange(count) 
        order <- order$player_name
        label_m <- "count"
        
      } else if (order_select == "win to loss differential") {
        order <- goalie_wl_filter |>  filter(outcome == "losses") |>  
          arrange(diff_wl) 
        order <- order$player_name
        label_m <- "diff_wl"
      } else {
        order <- goalie_wl_filter |> filter(outcome == "losses") |> 
          arrange(prop_w)
        order <- order$player_name
        label_m <- "prop_w"
      }
      
      # relevel data
      goalie_wl_filter$player_name <- factor(goalie_wl_filter$player_name, 
                                             levels = order)
      
      # adjust plot labels for 'wins' selection & no team selected
      if (order_select == "wins") {
        
        ggplot(goalie_wl_filter, aes(x = player_name, y = count, 
                                     fill = outcome)) +
          geom_bar(data = subset(goalie_wl_filter, outcome == "wins"), 
                   stat = "identity") +
          geom_bar(data = subset(goalie_wl_filter, outcome == "losses"),
                   aes(y = -count, fill = outcome), stat = "identity") +
          labs(title = "Losses and Wins by Goalie",
               x = "Goalie",
               y = "Count") +
          scale_fill_manual(values = c("wins" = "darkslateblue",
                                       "losses" = "indianred2"),
                            guide = guide_legend(title = NULL)) +
          theme_minimal() +
          theme(plot.title = element_text(face = "bold", size = 14, hjust=0.04),
                axis.text = element_text(size = 14),
                legend.text = element_text(size = 12),
                legend.position = "right",
                axis.title.x = element_text(size = 14),
                axis.title.y = element_text(size = 14), 
                plot.title.position = "plot") +
          coord_flip() +
          geom_text(data = subset(goalie_wl_filter, outcome == "wins"),
                    aes(label = !!sym(label_m), y = count), hjust = -0.5,
                    color = "black", size = 4) +
          geom_text(data = subset(goalie_wl_filter, outcome == "losses"),
                    aes(label = team, y = -count), hjust = 1.3,
                    color = "black", size = 4) +
          expand_limits(y = c(-8, 14))
        } else {
        
        # 'wins' not selected and no team selected   
        ggplot(goalie_wl_filter, aes(x = player_name, y = count, 
                                       fill = outcome)) +
        geom_bar(data = subset(goalie_wl_filter, outcome == "wins"), 
                 stat = "identity") +
        geom_bar(data = subset(goalie_wl_filter, outcome == "losses"),
                 aes(y = -count, fill = outcome), stat = "identity") +
        labs(title = "Losses and Wins by Goalie",
             x = "Goalie",
             y = "Count") +
        scale_fill_manual(values = c("wins" = "darkslateblue",
                                     "losses" = "indianred2"),
                          guide = guide_legend(title = NULL)) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 14, hjust=0.04),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 12),
              legend.position = "right",
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14), 
              plot.title.position = "plot") +
        coord_flip() +
        geom_text(data = subset(goalie_wl_filter, outcome == "losses"),
                  aes(label = !!sym(label_m), y = -count), hjust = 1.5,
                  color = "black", size = 4) +
        geom_text(data = subset(goalie_wl_filter, outcome == "wins"),
                  aes(label = team, y = count), hjust = -0.2,
                  color = "black", size = 4) +
        expand_limits(y = c(-8, 14))}
      
    } else { 
    
    # filter data for desired subset
    goalie_wl_filter <- subset(goalie_wl, team_full %in% input$team_select)
    
    order_select <- input$gwl_select

    # Set Ordering
    if (order_select == "wins") {

      order <- goalie_wl_filter |> filter(outcome == "wins") |> arrange(count)
      order <- order$player_name
      label_m <- "count"
      
    } else if (order_select == "losses") {
      order <- goalie_wl_filter |> filter(outcome == "losses") |> 
        arrange(count) 
      order <- order$player_name
      label_m <- "count"
      
    } else if (order_select == "win to loss differential") {
      order <- goalie_wl_filter |>  filter(outcome == "losses") |>  
        arrange(diff_wl) 
      order <- order$player_name
      label_m <- "diff_wl"
    } else {
      order <- goalie_wl_filter |> filter(outcome == "losses") |> 
        arrange(prop_w)
      order <- order$player_name
      label_m <- "prop_w"
    }
    
    # relevel data
    goalie_wl_filter$player_name <- factor(goalie_wl_filter$player_name, 
                                           levels = order)

    # adjust plot labels for 'wins' selected and teams selected 
    if (order_select == "wins"){
      ggplot(goalie_wl_filter, aes(x = player_name, y = count, fill = outcome)) +
        geom_bar(data = subset(goalie_wl_filter, outcome == "wins"),
                 stat = "identity") +
        geom_bar(data = subset(goalie_wl_filter, outcome == "losses"),
                 aes(y = -count, fill = outcome), stat = "identity") +
        labs(title = "Losses and Wins by Goalie",
             x = "Goalie",
             y = "Count") +
        scale_fill_manual(values = c("wins" = "darkslateblue",
                                     "losses" = "indianred2"),
                          guide = guide_legend(title = NULL)) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 14, hjust=0.04),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 12),
              legend.position = "right",
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14), 
              plot.title.position = "plot") +
        coord_flip() +
        geom_text(data = subset(goalie_wl_filter, outcome == "wins"),
                  aes(label = !!sym(label_m), y = count), hjust = -0.5,
                  color = "black", size = 4) +
        geom_text(data = subset(goalie_wl_filter, outcome == "losses"),
                  aes(label = team, y = -count), hjust = 1.3,
                  color = "black", size = 4) +
        expand_limits(y = c(-8, 14))
      
    } else {
      
      # 'wins not selected and teams selected 
      ggplot(goalie_wl_filter, aes(x = player_name, y = count, fill = outcome)) +
        geom_bar(data = subset(goalie_wl_filter, outcome == "wins"),
                 stat = "identity") +
        geom_bar(data = subset(goalie_wl_filter, outcome == "losses"),
                 aes(y = -count, fill = outcome), stat = "identity") +
        labs(title = "Losses and Wins by Goalie",
             x = "Goalie",
             y = "Count") +
        scale_fill_manual(values = c("wins" = "darkslateblue",
                                     "losses" = "indianred2"),
                          guide = guide_legend(title = NULL)) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 14, hjust=0.04),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 12),
              legend.position = "right",
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14), 
              plot.title.position = "plot") +
        coord_flip() +
        geom_text(data = subset(goalie_wl_filter, outcome == "losses"),
                  aes(label = !!sym(label_m), y = -count), hjust = 1.5,
                  color = "black", size = 4) +
        geom_text(data = subset(goalie_wl_filter, outcome == "wins"),
                  aes(label = team, y = count), hjust = -0.2,
                  color = "black", size = 4) +
        expand_limits(y = c(-8, 14))
      
    }
    }
  })
  
  # SAVE PERCENTAGE----------------------------------------------------------SP
  
  select_goalie <- reactiveVal(NULL)
  
  observeEvent(input$sp_table_rows_selected, {
    if (!is.null(input$sp_table_rows_selected)) {
      # Get the index of the selected row
      row_index <- input$sp_table_rows_selected
      
      # Get the value of the "player_name" from the selected row
      player <- save_goals[row_index, "player_name"]
      
      # Update the selected goalie value
      select_goalie(player)
    } else {
      select_goalie(NULL)
    }
  })

  output$sp_table <- renderDT({
    
    datatable(save_goals %>%
                select(team, player_name, save_percentage),
              rownames = NULL, 
              selection = "single",
              colnames = c("Team", "Goalie", "Save %"),
              options = list(paging = FALSE, 
                             info = FALSE, 
                             searching = FALSE)
    )
    
  }) 
  
  output$selected_goalie_summary <- renderText({
    selected <- select_goalie()
    if (!is.null(selected)) {
      goalie <- selected[[1]]
      
      summary_goalie <- save_goals |> filter(player_name == goalie) |> 
        select(goals_against_avg, games_played, shutouts)
      goalie_summary <- paste(
        "Goals Against Average:", summary_goalie$goals_against_avg, 
        "Games Played:", summary_goalie$games_played,
        "Shutouts:", summary_goalie$shutouts
      )
      
      return(goalie_summary)
      
    } else {
      return("")
    }
  })
  
  output$additional_text <- renderText({
    "Click on a goalie for additional stats. "
  })
  
  
}

shinyApp(ui, server)
