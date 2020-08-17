#' @title WNBA Ranking
#' @name wnba_ranking
#' @description Runs `dfstools::mvglmmRank` and `dfstools::game_predict` and
#' returns a list of the results
#' @export wnba_ranking
#' @param rank_prep the output of `dfstools::wnba_rank_prep_wnba` or
#' `dfstools::wnba_rank_prep_bbref`
#' @return a list of two items
#' \itemize{
#' \item rank_model the result of mvglmmRank_model
#' \item forecast the result of game_predict
#' }
#' @examples
#' \dontrun{
#' rank_prep_wnba <- dfstools::wnba_2020_rank_prep_wnba()
#' ranking_wnba <- dfstools::wnba_ranking(rank_prep_wnba)
#' rank_prep_bbref <- dfstools::wnba_2020_rank_prep_bbref()
#' ranking_bbref <- dfstools::wnba_ranking(rank_prep_bbref)
#' }

wnba_ranking <- function(rank_prep) {
  rank_model <- mvglmmRank_model(rank_prep$game.data)
  forecast <- game_predict(rank_prep$schedule, rank_model)
  return(list(rank_model = rank_model, forecast = forecast))
}

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
