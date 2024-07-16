options(shiny.port = 8050, shiny.autoreload = TRUE)
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(ggrepel)
library(plotly)
library(DT)
library(shinythemes)

team_ranking <- read_csv("data/team_ranking.csv") 
goalie_wl <- read_csv("data/goalie_wl.csv")
save_goals <- read_csv("data/save_goals.csv")

teams <- c("Boston", "Minnesota", "Montreal", "New York",  "Ottawa", "Toronto")
gwl_metrics <- c("wins", "losses", "win to loss differential", 
                 "proportion of wins (wins/games_played)")

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("body { background-color: #FFFFFF;}"))
  ),
  
  theme = shinytheme("yeti"),
  
  # Header
  fluidRow(
    column(
      width = 12,
      align = "center",
      h1("PWHL Goalie Tracker", 
         style = "color: white; font-size: 30px; 
                background: linear-gradient(135deg, darkslateblue, #9b59b6); 
                padding: 12px; border-radius: 8px; font-weight: 300;")
      )
  ),
  
  
  # Main layout with two columns
  fluidRow(
    
    # COLUMN 1
    column(
      width = 4,
      
      # team selection
      div(
        style = "background-color: #f2f2f2; padding: 10px; margin-bottom: 10px;
        border-radius: 8px;",
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
      
      div(
        style = "background-color: #f2f2f2; padding: 10px; margin-bottom: 5px;
        border-radius: 8px;",
      # table
      DTOutput("sp_table"), 
      p("Click on a goalie to display additional statistics.", 
        style = "font-style: italic; font-size: 11px"),
      div(
        style = "background-color: #FFFFFF",
        textOutput("selected_goalie_title"),
        style = "margin-bottom: 0px; font-weight: bold;"
      ),
      plotlyOutput("ind_plot", height = "130px")
      )
    ),

    # COLUMN 2 (PLOTS)
    column(
      width = 8,
      # Team Ranking plot
      div(
        style = "background-color: #f2f2f2; padding: 10px; margin-bottom: 15px;
        border-radius: 8px;",
        plotlyOutput("team_plot", height = "325px")
      ),
      
      # GWL plot and order select
      div( 
        style = "background-color: #f2f2f2; padding: 10px; border-radius: 8px;",
        plotOutput("gwl_plot", height = "325px"),
        selectizeInput(
          "gwl_select",
          " ", 
          choices = gwl_metrics,
          multi = FALSE,
          selected = "wins",
          width = "330px",
          options = list(
            placeholder = 'Sort by...',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  
  # team ranking plot --------------------------------------------------------
  output$team_plot <- renderPlotly({
    team_colors <- c("New York" = rgb(0, 170, 170, maxColorValue = 255),
                     "Boston" = "darkgreen",
                     "Toronto" = rgb(100, 149, 237, maxColorValue = 255),
                     "Montreal" = "darkred",
                     "Ottawa" = "red",
                     "Minnesota" = "darkblue")

    # Default Plot (all teams displayed)
    if (length(input$team_select) == 0) {

      # for annotations
      last_data <- team_ranking |>
        group_by(team) |>
        summarize(last_game_date = max(game_date),
                  last_total_points = max(total_points))
      
      last_data$x_numeric <- as.numeric(last_data$last_game_date)

      # main plot
      p <- ggplot(team_ranking) +
        geom_line(aes(x=game_date, y=total_points, color=team)) +
        scale_color_manual(values = team_colors) +
        geom_point(data = last_data,
                   aes(x = last_game_date, y = last_total_points, color = team),
                   size = 2) +
        labs(title = "Team Overall Standings",
             y = "Cumulated Points",
             x = "Date",
             color = "Team") +
        theme_minimal() +
        theme(axis.line = element_line(color = "black"),
              legend.position = "None",
              plot.title = element_text(face = "bold", size = 10),
              axis.text = element_text(size = 10),
              axis.title.x = element_text(size = 10),
              axis.title.y = element_text(size = 10)) +
        scale_x_date(date_labels = "%b")
      
      # Add annotations & hover
      p <- ggplotly(p) |> 
        add_annotations(
          data = last_data,
          x = ~x_numeric,
          y = ~last_total_points,
          text = ~team,
          xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 0,
          arrowcolor = "lightgrey",
          ax = 60,
          ay = c(10, -5)
        ) |> 
        layout(showlegend = FALSE,
               xaxis = list(range = as.numeric(c(as.Date('2024-01-01'), 
                                                 as.Date('2024-06-01')))))
      p
  } 
    else {
      
      # filter data for desired subset
      filtered_data <- subset(team_ranking, team %in% input$team_select)
      
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
      last_data$x_numeric <- as.numeric(last_data$last_game_date)

      # main plot
      p <- ordered_data |>
        ggplot() +
        geom_line(aes(x=game_date, y=total_points, color=team)) +
        scale_color_manual(values = team_colors_subset) + 
        geom_point(data = last_data,
                   aes(x = last_game_date, y = last_total_points,
                       color = team),
                   size = 2) +
        labs(title = "Team Overall Standings",
             y = "Cumulated Points",
             x = "Date",
             color = "Team") +
        theme_minimal() +
        theme(axis.line = element_line(color = "black"),
              legend.position = "None",
              plot.title = element_text(face = "bold", size = 10),
              axis.text = element_text(size = 10),
              axis.title.x = element_text(size = 10),
              axis.title.y = element_text(size = 10)) +
        scale_x_date(date_labels = "%b")

      # Add annotations
      # check for tied teams 
      if (anyDuplicated(last_data$last_total_points) > 0) {
        ay_values <- c(5, -5)  
      } else {
        ay_values <- 0  
      }
      
      p <- ggplotly(p) |> 
        add_annotations(
          data = last_data,
          x = ~x_numeric,
          y = ~last_total_points,
          text = ~team,
          xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 0,
          arrowcolor = "lightgrey",
          ax = 60,
          ay = ay_values
        ) |> 
        layout(showlegend = FALSE,
               xaxis = list(range = as.numeric(c(as.Date('2024-01-01'), 
                                                 as.Date('2024-06-01')))))
      p
    }
  })

  # Goalie win/loss plot  ----------------------------------------------------
  output$gwl_plot <- renderPlot({ 
    
    # check if teams are selected (Default: plot all goalies)
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
                  aes(label = !!sym(label_m), y = -count), hjust = 1.2,
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
      
      # 'wins' not selected and teams selected 
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
        expand_limits(y = c(-8, 14))
      
    }
    }
  })
  
  # Save % Table ------------------------------------------------------------
  select_goalie <- reactiveVal(NULL)
  
  observeEvent(input$sp_table_rows_selected, {
    if (!is.null(input$sp_table_rows_selected)) {
      
      row_index <- input$sp_table_rows_selected
      
      # player at row selected
      player <- save_goals[row_index, "player_name"]
      
      # update goalie
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
              colnames = c("Team", "Goalie", "Save Percentage"),
              options = list(paging = FALSE, 
                             info = FALSE, 
                             searching = FALSE)
    )  
    })
                             
  
  # Individual Goalie Plot -----------------------------------------------------
  output$ind_plot <- renderPlotly({ 
    
    # average goalie stats
    avg_stats <- save_goals |> 
      summarise(across(where(is.numeric), ~round(mean(., na.rm = TRUE), 2)))

    goalie <- select_goalie()  
    
    # check if goalie selected 
    if (!is.null(goalie)) {
      
      goalie <- goalie[[1]]

      # selected goalie stats 
      goalie_stats <- save_goals %>% 
        filter(player_name == goalie)
      
      
      goalie_data <- data.frame(
        Metric = c("Goals Against (avg/game)", "Shutouts", "Games Played"),
        Value = c(goalie_stats$goals_against_avg, 
                   goalie_stats$shutouts, 
                   goalie_stats$games_played),
        Average = c(avg_stats$goals_against_avg, 
                    avg_stats$shutouts, 
                    avg_stats$games_played)
      )
      
      
      goalie_plot <- ggplot(goalie_data, aes(y = Metric)) +
        geom_col(aes(x = Value), fill = "steelblue2", width = 0.5) + 
        geom_point(aes(x = Average), color = "darkred", size = 2, shape = 18) + 
        labs(title = NULL,
             y = NULL,
             x = NULL) +
        theme_minimal() +
        theme(axis.text = element_text(size = 8)) + 
        xlim(0, 20)
      
      return(ggplotly(goalie_plot, tooltip = c("Value", "Average")))
      
    } else {
      
      goalie_data <- data.frame(
        Metric = c("Goals Against (avg/game)", "Shutouts", "Games Played"),
        Average = c(avg_stats$goals_against_avg, 
                    avg_stats$shutouts, 
                    avg_stats$games_played)
      )
      
      goalie_plot <- ggplot(goalie_data, aes(y = Metric)) +
        geom_point(aes(x = Average), color = "darkred", size = 2, shape = 18) + 
        labs(title = NULL,
             y = NULL,
             x = NULL) +
        theme_minimal() +
        theme(axis.text = element_text(size = 8)) + 
        xlim(0, 20)
      
      return(ggplotly(goalie_plot, tooltip = c("Average")))
      
    }
    
  })
  
  output$selected_goalie_title <- renderText({
    selected <- select_goalie()
    if (!is.null(selected)) {
      goalie <- selected[[1]]

      goalie_title <- paste(
        "Statistics for", goalie, "with Goalie Averages"
      )

      return(goalie_title)

    } else {
      return("Goalie Averages")
    }
  })

}

shinyApp(ui, server)
