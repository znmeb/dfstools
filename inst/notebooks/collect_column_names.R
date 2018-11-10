library(dplyr)
library(dfstools)
library(snakecase)
library(tibble)

column_name_table <- tibble()
for (league in c("nba", "nhl", "nfl", "mlb")) {
  table <- "games"
  print(paste(league, table))
  games <- msf_seasonal_games(
    league, season, verbose = TRUE)$games
  column_names <- games %>%
    colnames() %>% to_snake_case()
  rows <- tibble(league, season, table, column_names)
  column_name_table <- column_name_table %>%
    bind_rows(rows)

  teams <- games %>%
    dplyr::select(team = schedule_home_team_abbreviation) %>%
    dplyr::distinct()
  team <- teams[1]$team
  print(team)

  table <- "players"
  print(paste(league, table))
  column_names <- msf_seasonal_players(
    league, season, verbose = TRUE)$players %>%
    colnames() %>% to_snake_case()
  rows <- tibble(league, season, table, column_names)
  column_name_table <- column_name_table %>%
    bind_rows(rows)

  table <- "dfs"
  print(paste(league, table))
  column_names <- msf_seasonal_dfs(
    league, season, team, verbose = TRUE) %>%
    colnames() %>% to_snake_case()
  rows <- tibble(league, season, table, column_names)
  column_name_table <- column_name_table %>%
    bind_rows(rows)

  table <- "player_gamelogs"
  print(paste(league, table))
  column_names <- msf_seasonal_player_gamelogs(
    league, season, team, verbose = TRUE)$player_gamelogs %>%
    colnames() %>% to_snake_case()
  rows <- tibble(league, season, table, column_names)
  column_name_table <- column_name_table %>%
    bind_rows(rows)

  table <- "team_gamelogs"
  print(paste(league, table))
  column_names <- msf_seasonal_team_gamelogs(
    league, season, team, verbose = TRUE)$team_gamelogs %>%
    colnames() %>% to_snake_case()
  rows <- tibble(league, season, table, column_names)
  column_name_table <- column_name_table %>%
    bind_rows(rows)

}
