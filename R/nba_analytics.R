# utility function to select a standard set of player totals.
.nba_totals_select <- function(nba_player_totals) {
  return(nba_player_totals %>%
           dplyr::select(
             player_name:player_rookie,
             games_played = stats_games_played,
             total_rebounds = stats_rebounds_reb,
             total_minutes = stats_minutes_played,
             two_point_field_goals = stats_field_goals_fg_2_pt_made,
             three_point_field_goals = stats_field_goals_fg_3_pt_made,
             free_throws = stats_free_throws_ft_made,
             assists = stats_offense_ast,
             turnovers = stats_defense_tov,
             blocks = stats_defense_blk,
             steals = stats_defense_stl
           )
  )
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
#' @param season a valid MySportsFeeds v2.1 API season name
#' @return a tibble of season total box score statistics, arranged by
#' descending total points scored
#' @examples
#' \dontrun{
#' player_totals <- dfstools::nba_player_season_totals("current")
#' }

nba_player_season_totals <- function(season) {
  raw_data <- dfstools::msf_seasonal_player_stats_totals(
    league = "nba",
    season = season,
    verbose = FALSE
  ) %>%
    dplyr::filter(player_current_roster_status == "ROSTER") %>%
    dplyr::mutate(
      player_name =
        paste(player_first_name, player_last_name, player_primary_position),
      player_height_ft = .feet_inches_to_ft(player_height),
      stats_minutes_played = stats_miscellaneous_min_seconds / 60.0
    )
  label_columns <- c(
    "player_name",
    "player_current_team_abbreviation",
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
  stats_columns <- c(
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
    dplyr::group_by(player_name) %>%
    dplyr::summarize_at(
      .vars = dplyr::vars(stats_columns),
      .funs = dplyr::funs(sum)
    ) %>%
    dplyr::arrange(player_name) %>%
    dplyr::ungroup()
  return(
    labels %>%
      dplyr::full_join(totals) %>%
      dplyr::arrange(desc(stats_offense_pts))
  )
}

#' @title NBA Archetypal Analysis
#' @name nba_archetypes
#' @description perform an "archetypal athletes" analysis
#' @importFrom archetypes archetypes
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom tibble as_tibble
#' @export nba_archetypes
#' @param player_totals a tibble returned by `nba_player_season_totals`
#' @param num_archetypes number of archetypes to use (default 3)
#' @return a list of
#' \itemize{
#' \item archetype_parameters the parameters that define each archetype
#' \item player_alphas the players tagged with their loadings on each archetype
#' \item archetype_model the model object}
#' @examples
#' \dontrun{
#' dfstools::msf_set_apikey("your MySportsFeeds API key")
#' player_totals <- dfstools::nba_player_season_totals("2018-2019-regular")
#' the_archetypes <- dfstools::nba_archetypes(player_totals)
#' player_alphas <- the_archetypes[["player_alphas"]]
#' View(player_alphas)
#' }

nba_archetypes <- function(player_totals, num_archetypes = 3) {
  player_labels <- player_totals %>%
    dplyr::select(player_name:player_rookie)
  call_player_totals <- player_totals %>%
    dplyr::select(player_name, stats_minutes_played:stats_miscellaneous_fouls)
  return(compute_archetypes(
    call_player_totals, player_labels, num_archetypes
  ))
}

#' @title NBA Archetype Search
#' @name nba_archetype_search
#' @description stepwise search of archetype counts
#' @importFrom archetypes stepArchetypes
#' @importFrom archetypes bestModel
#' @importFrom archetypes robustArchetypes
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom tibble as_tibble
#' @export nba_archetype_search
#' @param player_totals a tibble returned by `nba_player_season_totals`
#' @param num_steps number of steps to use (default 1:5)
#' @param nrep number of repetitions at each step (default 4)
#' @param verbose should the search be verbose? (default FALSE)
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
#' player_totals <- dfstools::nba_player_season_totals("2018-2019-regular")
#' the_archetypes <- dfstools::nba_archetype_search(player_totals)
#' screeplot(the_archetypes$archetype_models)
#' player_alphas <- the_archetypes[["player_alphas"]]
#' View(player_alphas)
#' }

nba_archetype_search <-
  function(player_totals, num_steps = 1:7, nrep = 32, verbose = FALSE) {
    player_labels <- player_totals %>%
      dplyr::select(player_name:player_rookie)
    call_player_totals <- player_totals %>%
      dplyr::select(player_name, stats_minutes_played:stats_miscellaneous_fouls)
    return(archetype_search(
      call_player_totals, player_labels, num_steps, nrep, verbose
    ))
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
