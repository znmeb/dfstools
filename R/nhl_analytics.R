#  Text feet and inches to feet and decimals
.feet_inches_to_ft <- function(height) {
  feet <- as.numeric(sub("\'.*$", "", height))
  inches <- sub("^.*\'", "", height)
  inches <- as.numeric(sub("\".*$", "", inches))
  return(feet + inches / 12.0)
}
library(dfstools)
library(dplyr)
source("~/.ssh/msf_key")
nhl_raw <- dfstools::msf_seasonal_player_stats_totals("nhl", "2018-2019-regular") %>%
  dplyr::filter(player_current_roster_status == "ROSTER") %>%
  dplyr::mutate(
    player_name = paste(player_first_name, player_last_name),
    player_height_ft = .feet_inches_to_ft(player_height)
  )
goalies <- nhl_raw %>% dplyr::filter(player_primary_position == "G") %>%
  dplyr::mutate(stats_minutes_played = stats_goaltending_minutes_played) %>%
  dplyr::arrange(desc(stats_minutes_played))
skaters <- nhl_raw %>% dplyr::filter(player_primary_position != "G") %>%
  dplyr::mutate(stats_minutes_played = stats_shifts_time_on_ice_seconds / 60.0) %>%
  dplyr::arrange(desc(stats_minutes_played))
label_columns <- c(
  "player_name",
  "player_primary_position",
  "player_current_team_abbreviation",
  "player_height",
  "player_height_ft",
  "player_weight",
  "player_birth_date",
  "player_age",
  "player_rookie"
)
labels <- nhl_raw %>%
  dplyr::select(label_columns) %>%
  dplyr::arrange(player_name) %>%
  unique()

goalie_stats_columns <- c(
  "stats_minutes_played",
  "stats_games_played",
  grep("_goaltending_", names(goalies), value = TRUE) %>%
    grep("percent", ., value = TRUE, invert = TRUE)
)

skater_stats_columns <- c(
  "stats_minutes_played",
  "stats_games_played",
  grep("_scoring_", names(goalies), value = TRUE) %>%
    grep("percent", ., value = TRUE, invert = TRUE),
  grep("_skating_", names(goalies), value = TRUE) %>%
    grep("percent", ., value = TRUE, invert = TRUE),
  grep("_shifts_", names(goalies), value = TRUE)
)

goalie_totals <- goalies %>%
  dplyr::group_by(player_name) %>%
  dplyr::summarize_at(
    .vars = dplyr::vars(goalie_stats_columns),
    .funs = dplyr::funs(sum)
  ) %>%
  dplyr::arrange(player_name) %>%
  dplyr::ungroup()

skater_totals <- skaters %>%
  dplyr::group_by(player_name) %>%
  dplyr::summarize_at(
    .vars = dplyr::vars(skater_stats_columns),
    .funs = dplyr::funs(sum)
  ) %>%
  dplyr::arrange(player_name) %>%
  dplyr::ungroup()

