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
#' @return a list of three items
#' \itemize{
#' \item goalie_totals a tibble of goalie season total box score statistics,
#' arranged by descending games won
#' \item skater_totals a tibble of skater season total box score statistics,
#' arranged by descending goals made
#' \item player_labels a tibble of labeling information for both goalies and
#' skaters
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
  player_labels <- raw_data %>%
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

  return(list(
    goalie_totals = goalie_totals,
    skater_totals = skater_totals,
    player_labels = player_labels
  ))

}

#' @title NHL Skater Archetype Search
#' @name nhl_skater_archetype_search
#' @description stepwise search of archetype counts
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @export nhl_skater_archetype_search
#' @param skater_totals a tibble returned by `nhl_player_season_totals`
#' @param num_steps number of steps to use (default 1:5)
#' @param nrep number of repetitions at each step (default 4)
#' @param verbose should the search be verbose? (default TRUE)
#' @return a list of
#' \itemize{
#' \item archetype_parameters the parameters that define each archetype
#' \item player_alphas the players tagged with their loadings on each archetype
#' \item archetype_model the model object - the `bestModel` with `num_steps`
#' archetypes
#' \item all of the models}
#' @examples
#' \dontrun{
#' dfstools::msf_set_apikey("your MySportsFeeds API key")
#' player_totals <- dfstools::nhl_player_season_totals("2018-2019-regular")
#' goalie_totals <- player_totals$goalie_totals
#' skater_totals <- player_totals$skater_totals
#' skater_archetypes <- dfstools::nhl_skater_archetype_search(skater_totals)
#' screeplot(skater_archetypes$archetype_models)
#' skater_alphas <- skater_archetypes[["player_alphas"]]
#' View(skater_alphas)
#' }

nhl_skater_archetype_search <-
  function(skater_totals, num_steps = 1:7, nrep = 32, verbose = TRUE) {
    return(archetype_search(skater_totals, num_steps, nrep, verbose))
  }

#' @title NHL Goalie Archetype Search
#' @name nhl_goalie_archetype_search
#' @description stepwise search of archetype counts
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @export nhl_goalie_archetype_search
#' @param goalie_totals a tibble returned by `nhl_player_season_totals`
#' @param num_steps number of steps to use (default 1:7)
#' @param nrep number of repetitions at each step (default 32)
#' @param verbose should the search be verbose? (default TRUE)
#' @return a list of
#' \itemize{
#' \item archetype_parameters the parameters that define each archetype
#' \item player_alphas the players tagged with their loadings on each archetype
#' \item archetype_model the model object - the `bestModel` with `num_steps`
#' archetypes
#' \item all of the models}
#' @examples
#' \dontrun{
#' dfstools::msf_set_apikey("your MySportsFeeds API key")
#' player_totals <- dfstools::nhl_player_season_totals("2018-2019-regular")
#' goalie_totals <- player_totals$goalie_totals
#' skater_totals <- player_totals$skater_totals
#' goalie_archetypes <- dfstools::nhl_goalie_archetype_search(goalie_totals)
#' screeplot(goalie_archetypes$archetype_models)
#' goalie_alphas <- goalie_archetypes[["player_alphas"]]
#' View(goalie_alphas)
#' }

nhl_goalie_archetype_search <-
  function(goalie_totals, num_steps = 1:7, nrep = 32, verbose = TRUE) {
    return(archetype_search(goalie_totals, num_steps, nrep, verbose))
  }


utils::globalVariables(c(
  "player_primary_position",
  "stats_goaltending_minutes_played",
  "stats_goaltending_wins",
  "stats_scoring_goals",
  "stats_shifts_time_on_ice_seconds"
))
