#  Text feet and inches to centimeters
.feet_inches_to_cm <- function(height) {
  feet <- as.numeric(sub("\'.*$", "", height))
  inches <- sub("^.*\'", "", height)
  inches <- as.numeric(sub("\".*$", "", inches))
  return(2.54 * (12.0 * feet + inches))
}

#' @title NBA Player Season Totals
#' @name nba_player_season_totals
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
#' @export nba_player_season_totals
#' @param season a valid MySportsFeeds v2.0 API season name
#' @return a tibble of season total box score statistics, arranged by
#' descending total points scored
#' @examples
#' \dontrun{
#' player_totals <- dfstools::nba_player_season_totals("current")
#' }

nba_player_season_totals <- function(season) {
  response <- msf_seasonal_player_stats_totals(
    league = "nba",
    season = season,
    verbose = FALSE
  )
  raw_data <- response[["player_stats_totals"]] %>%
    dplyr::filter(player_current_roster_status == "ROSTER") %>%
    dplyr::mutate(
      player_name = paste(player_first_name, player_last_name),
      player_height_cm = .feet_inches_to_cm(player_height),
      stats_minutes_played = stats_miscellaneous_min_seconds / 60.0
    )
  label_columns <- c(
    "player_name",
    "player_primary_position",
    "player_height",
    "player_height_cm",
    "player_weight",
    "player_birth_date",
    "player_age",
    "player_rookie"
  )
  stats_columns <- c(
    "player_name",
    "stats_minutes_played",
    "stats_games_played",
    "stats_miscellaneous_games_started",
    "stats_field_goals_fg_2_pt_att",
    "stats_field_goals_fg_2_pt_made",
    "stats_field_goals_fg_3_pt_att",
    "stats_field_goals_fg_3_pt_made",
    "stats_field_goals_fg_att",
    "stats_field_goals_fg_made",
    "stats_free_throws_ft_att",
    "stats_free_throws_ft_made",
    "stats_rebounds_off_reb",
    "stats_rebounds_def_reb",
    "stats_rebounds_reb",
    "stats_offense_ast",
    "stats_offense_pts",
    "stats_defense_tov",
    "stats_defense_stl",
    "stats_defense_blk",
    "stats_miscellaneous_fouls"
  )
  totals <- raw_data %>%
    dplyr::select_at(.vars = stats_columns) %>%
    dplyr::group_by(
      player_name
    ) %>%
    dplyr::summarize_at(
      .vars = dplyr::vars(stats_minutes_played:stats_miscellaneous_fouls),
      .funs = dplyr::funs(sum)
    ) %>%
    dplyr::ungroup()
  labels <- raw_data %>%
    dplyr::select_at(.vars = label_columns) %>%
    unique()
  return(
    dplyr::full_join(labels, totals) %>%
      dplyr::arrange(desc(stats_offense_pts))
  )
}

utils::globalVariables(c(
  "player_current_roster_status",
  "player_first_name",
  "player_height",
  "player_last_name",
  "player_name",
  "stats_minutes_played",
  "stats_miscellaneous_fouls",
  "stats_miscellaneous_min_seconds",
  "stats_offense_pts"
))
