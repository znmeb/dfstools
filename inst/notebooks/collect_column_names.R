library(dplyr)
library(dfstools)
library(tibble)

all.na <- function(x) all(is.na(x))
new_rows <- function(df) {
  all_na <- sapply(df, all.na)
  type_of <- sapply(df, typeof)
  rows <- tibble(
    league, season, table, colnames(df), all_na, type_of
  )
  return(rows)
}

column_name_table <- tibble()
season <- "current"

for (league in c("nba", "nhl", "nfl")) {
  table <- "games"
  print(paste(league, table))
  games <- msf_seasonal_games(
    league, season, verbose = TRUE)$games
  column_name_table <- column_name_table %>%
    bind_rows(new_rows(games))

  teams <- games %>%
    dplyr::select(team = schedule_home_team_abbreviation) %>%
    dplyr::distinct()
  team <- teams$team[1]
  print(team)

  table <- "players"
  print(paste(league, table))
  players <- msf_seasonal_players(
    league, season, verbose = TRUE)$players
  column_name_table <- column_name_table %>%
    bind_rows(new_rows(players))

  table <- "dfs"
  print(paste(league, table))
  dfs <- msf_seasonal_dfs(
    league, season, team, verbose = TRUE)$dfs
  column_name_table <- column_name_table %>%
    bind_rows(new_rows(dfs))

  table <- "player_gamelogs"
  print(paste(league, table))
  player_gamelogs <- msf_seasonal_player_gamelogs(
    league, season, team, verbose = TRUE)$player_gamelogs
  column_name_table <- column_name_table %>%
    bind_rows(new_rows(player_gamelogs))

  table <- "team_gamelogs"
  print(paste(league, table))
  team_gamelogs <- msf_seasonal_team_gamelogs(
    league, season, team, verbose = TRUE)$team_gamelogs
  column_name_table <- column_name_table %>%
    bind_rows(new_rows(team_gamelogs))

}
