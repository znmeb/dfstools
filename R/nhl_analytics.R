#  Text feet and inches to feet and decimals
.feet_inches_to_ft <- function(height) {
  feet <- as.numeric(sub("\'.*$", "", height))
  inches <- sub("^.*\'", "", height)
  inches <- as.numeric(sub("\".*$", "", inches))
  return(feet + inches / 12.0)
}

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

