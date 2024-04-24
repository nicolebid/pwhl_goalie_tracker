library(fastRhockey)
library(readr)
library(dplyr)
library(tidyr)
library(here)

# TEAM RANKING DATA
overall_standings <- pwhl_standings(season = 2023, regular = TRUE)

# preprocess: find cumulated sum of points for ranking 
results <- pwhl_schedule(2023) |> 
  select(-venue, -venue_url)
results$game_date <- as.Date(results$game_date, format = "%a, %b %d")

results_by_team <- results |> 
  pivot_longer(cols = c(home_team, away_team), values_to = "team") |> 
  select(-name) |> 
  mutate(game_pts = case_when(game_status == "Final" & winner == team  ~ 3, 
                              game_status == "Final" & winner != team  ~ 0, 
                              game_status != "Final" & winner == team ~ 2, 
                              game_status != "Final" & winner != team ~ 1 )) |> 
  filter(game_date <= as.Date(Sys.Date())) |> 
  group_by(team) |> 
  mutate(total_points = cumsum(game_pts), 
         games_played = row_number()) 
print(results_by_team)

# write as csv
write_csv(results_by_team, here("data", "team_ranking.csv"))


# GOALIE WINS/LOSSES DATA
goalie_stats <- pwhl_stats() |> 
  arrange(wins) |> 
  mutate(
    wins = as.numeric(wins),
    losses = as.numeric(losses),
    diff_wl = wins - losses, 
    prop_w = round(wins / (wins + losses), 2)) |> 
  drop_na() |> 
  mutate(team_full = case_when(
    team == "NY" ~ "New York",
    team == "MTL" ~ "Montreal",
    team == "TOR" ~ "Toronto",
    team == "BOS" ~ "Boston",
    team == "MIN" ~ "Minnesota",
    team == "OTT" ~ "Ottawa"))

goalie_wl <- goalie_stats |> 
  select(c("player_id", "player_name", "wins", 
           "losses", "team", "diff_wl", "prop_w", "team_full")) |> 
  pivot_longer(cols = c(wins, losses), 
               names_to = "outcome", 
               values_to = "count") |> 
  arrange(player_name, desc(count))

goalie_wl$count <- as.numeric(goalie_wl$count)

# write as csv
write_csv(goalie_wl, here("data", "goalie_wl.csv"))


# GOALIE ADDITIONAL DATA
save_goals <- pwhl_stats() |> 
  select(player_name, team, save_percentage, goals_against_avg, games_played, shutouts) |> 
  mutate(team_full = case_when(
    team == "NY" ~ "New York",
    team == "MTL" ~ "Montreal",
    team == "TOR" ~ "Toronto",
    team == "BOS" ~ "Boston",
    team == "MIN" ~ "Minnesota",
    team == "OTT" ~ "Ottawa"))

# write as csv
write_csv(save_goals, here("data", "save_goals.csv"))

