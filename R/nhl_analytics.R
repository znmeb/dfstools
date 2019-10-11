#' @title NHL Player Season Totals
#' @name nhl_player_season_totals
#' @description fetches the player season totals for base box score statistics
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select_at
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr arrange
#' @importFrom dplyr vars
#' @importFrom dplyr funs
#' @importFrom dplyr full_join
#' @importFrom dplyr desc
#' @export nhl_player_season_totals
#' @param season a valid MySportsFeeds v2.1 API season name
#' @return a list of two items
#' \itemize{
#' \item goalie_totals a tibble of goalie season total box score statistics,
#' arranged by descending games won
#' \item skater_totals a tibble of skater season total box score statistics,
#' arranged by descending goals made
#' }
#' @examples
#' \dontrun{
#' player_totals <- dfstools::nhl_player_season_totals("2018-2019-regular")
#' goalie_totals <- player_totals$goalie_totals
#' skater_totals <- player_totals$skater_totals
#' }

nhl_player_season_totals <- function(season) {

  raw_data <- dfstools::msf_seasonal_player_stats_totals("nhl", season) %>%
    dplyr::filter(player_current_roster_status == "ROSTER") %>%
    dplyr::mutate(
      player_name = paste(
        player_first_name, player_last_name, player_primary_position,
        player_current_team_abbreviation
      ),
      player_height_ft = .feet_inches_to_ft(player_height)
    )
  goalies <- raw_data %>% dplyr::filter(player_primary_position == "G") %>%
    dplyr::mutate(stats_minutes_played = stats_goaltending_minutes_played)
  skaters <- raw_data %>% dplyr::filter(player_primary_position != "G") %>%
    dplyr::mutate(stats_minutes_played = stats_shifts_time_on_ice_seconds / 60.0)
  label_columns <- c(
    "player_name",
    "player_height",
    "player_height_ft",
    "player_weight",
    "player_birth_date",
    "player_age",
    "player_rookie"
  )
  labels <- raw_data %>%
    dplyr::select(label_columns) %>%
    dplyr::arrange(player_name) %>%
    unique()

  goalie_stats_columns <- c(
    "stats_minutes_played",
    "stats_games_played",
    grep("_goaltending_", names(goalies), value = TRUE) %>%
      grep("percent", ., value = TRUE, invert = TRUE) %>%
      grep("stats_goaltending_minutes_played", ., value = TRUE, invert = TRUE)
  )

  skater_stats_columns <- c(
    "stats_minutes_played",
    "stats_games_played",
    grep("_scoring_", names(skaters), value = TRUE) %>%
      grep("percent", ., value = TRUE, invert = TRUE),
    grep("_skating_", names(goalies), value = TRUE) %>%
      grep("percent", ., value = TRUE, invert = TRUE),
    grep("_shifts_", names(goalies), value = TRUE) %>%
      grep("stats_shifts_time_on_ice_seconds", ., value = TRUE, invert = TRUE)
  )

  goalie_totals <- goalies %>%
    dplyr::group_by(player_name) %>%
    dplyr::summarize_at(
      .vars = dplyr::vars(goalie_stats_columns),
      .funs = sum
    ) %>%
    dplyr::arrange(desc(stats_goaltending_wins)) %>%
    dplyr::ungroup() %>%
    dplyr::select_if(.predicate = .is_valid_column)

  skater_totals <- skaters %>%
    dplyr::group_by(player_name) %>%
    dplyr::summarize_at(
      .vars = dplyr::vars(skater_stats_columns),
      .funs = sum
    ) %>%
    dplyr::arrange(desc(stats_scoring_goals)) %>%
    dplyr::ungroup() %>%
    dplyr::select_if(.predicate = .is_valid_column)

  return(list(goalie_totals = goalie_totals, skater_totals = skater_totals))

}

utils::globalVariables(c(
  "player_primary_position",
  "stats_goaltending_minutes_played",
  "stats_goaltending_wins",
  "stats_scoring_goals",
  "stats_shifts_time_on_ice_seconds"
))
