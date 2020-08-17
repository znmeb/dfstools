#' @title WNBA Archetypal Analysis
#' @name wnba_archetypes
#' @description perform an "archetypal athletes" analysis
#' @importFrom archetypes archetypes
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom tibble as_tibble
#' @export wnba_archetypes
#' @param player_totals a tibble returned by `nba_player_season_totals`
#' @param num_archetypes number of archetypes to use (default 3)
#' @return a list of
#' \itemize{
#' \item archetype_parameters the parameters that define each archetype
#' \item player_alphas the players tagged with their loadings on each archetype
#' \item archetype_model the model object}
#' @examples
#' \dontrun{
#' wnba_totals <- dfstools::wnba_season_totals_bbref(2020)
#' player_totals <- wnba_totals$player_totals
#' player_labels <- wnba_totals$player_labels
#' the_archetypes <- dfstools::wnba_archetypes(player_totals)
#' player_alphas <- the_archetypes[["player_alphas"]]
#' archetype_parameters <- the_archetypes[["archetype_parameters"]]
#' View(player_alphas)
#' View(archetype_parameters)
#' }

wnba_archetypes <- function(player_totals, num_archetypes = 3) {
  return(compute_archetypes(player_totals, num_archetypes))
}

#' @title WNBA Archetype Search
#' @name wnba_archetype_search
#' @description stepwise search of archetype counts
#' @importFrom archetypes stepArchetypes
#' @importFrom archetypes bestModel
#' @importFrom archetypes robustArchetypes
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom tibble as_tibble
#' @export wnba_archetype_search
#' @param player_totals a tibble returned by `nba_player_season_totals`
#' @param num_steps number of steps to use (default 1:10)
#' @param nrep number of repetitions at each step (default 64)
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
#' wnba_totals <- dfstools::wnba_season_totals_bbref(2020)
#' player_totals <- wnba_totals$player_totals
#' player_labels <- wnba_totals$player_labels
#' the_archetypes <- dfstools::wnba_archetype_search(player_totals)
#' screeplot(the_archetypes$archetype_models)
#' }

wnba_archetype_search <-
  function(player_totals, num_steps = 1:10, nrep = 64, verbose = TRUE) {
    return(archetype_search(player_totals, num_steps, nrep, verbose))
  }

#' @title Make WNBA `game_predict` schedule
#' @name wnba_make_game_predict_schedule
#' @description Builds a `game_predict` `schedule` table from a
#' `wnba_2020_schedule` result
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @export wnba_make_game_predict_schedule
#' @param wnba_schedule a tibble returned from `wnba_2020_schedule`
#' @return a `schedule` table
#' @examples
#' \dontrun{
#' wnba_schedule <- dfstools::wnba_2020_schedule()
#' wnba_game_data <- dfstools::wnba_make_game_data(wnba_schedule)
#' wnba_game_predict_schedule <-
#'   dfstools::wnba_make_game_predict_schedule(wnba_schedule)
#' View(wnba_game_data)
#' View(wnba_game_predict_schedule)
#' }

wnba_make_game_predict_schedule <- function(wnba_schedule) {
  wnba_schedule %>% dplyr::filter(is.na(away.response))
}

#' @title Make WNBA game.data
#' @name wnba_make_game_data
#' @description Builds a `mvglmmRank_model` `game.data` table from a
#' `wnba_2020_schedule` result
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @export wnba_make_game_data
#' @param wnba_schedule a tibble returned from `wnba_2020_schedule`
#' @return a `game.data` table
#' @examples
#' \dontrun{
#' wnba_schedule <- dfstools::wnba_2020_schedule()
#' wnba_game_data <- dfstools::wnba_make_game_data(wnba_schedule)
#' wnba_game_predict_schedule <-
#'   dfstools::wnba_make_game_predict_schedule(wnba_schedule)
#' View(wnba_game_data)
#' View(wnba_game_predict_schedule)
#' }

wnba_make_game_data <- function(wnba_schedule) {
  wnba_schedule %>% dplyr::filter(!is.na(away.response))
}

#' @title Rest days
#' @name wnba_rest_days
#' @description Compute rest days from the schedule
#' @export wnba_rest_days
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_rows
#' @importFrom dplyr lag
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @param schedule a tibble returned by `wnba_2020_schedule_bbref`
#' @return a tibble with three columns
#' \itemize{
#' \item `team` character team name,
#' \item `date` POSIXct the game date,
#' \item `rest_days` numeric number of rest days
#' }
#' @examples
#' \dontrun{
#' wnba_schedule <- dfstools::wnba_2020_schedule_bbref()
#' View(wnba_schedule)
#' rest_days <- dfstools::wnba_rest_days(wnba_schedule)
#' View(rest_days)
#' }

wnba_rest_days <- function(schedule) {
  aways <- schedule %>% dplyr::select(team = away, date)
  homes <- schedule %>% dplyr::select(team = home, date)
  aways %>% dplyr::bind_rows(homes) %>%
    dplyr::arrange(team, date) %>%
    dplyr::group_by(team) %>%
    dplyr::mutate(rest_days = as.numeric(date - dplyr::lag(date)) - 1) %>%
    dplyr::ungroup()
}

## global name declarations - See
## https://github.com/STAT545-UBC/Discussion/issues/451#issuecomment-264598618
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
  "binary.response",
  "fg3a",
  "fg3m",
  "fga",
  "fgm",
  "gdte",
  "h.s",
  "h.tc",
  "h.tn",
  "orb",
  "player",
  "pts",
  "raw_date",
  "team",
  "tm",
  "trb",
  "v.s",
  "v.tc",
  "v.tn"
))
