#' @title WNBA Ranking
#' @name wnba_ranking
#' @description Runs `dfstools::mvglmmRank` and `dfstools::game_predict` and
#' returns a list of the results
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @importFrom tibble tribble
#' @export wnba_ranking
#' @param rank_prep the output of `dfstools::wnba_rank_prep_wnba` or
#' `dfstools::wnba_rank_prep_bbref`
#' @return a list of five items
#' \itemize{
#' \item rank_model the result of mvglmmRank_model
#' \item offensive ratings
#' \item defensive ratings
#' \item forecast the result of game_predict
#' \item daily_entropy the projected entropy for upcoming dates
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

  offensive_ratings <- tibble::as_tibble(list(
    team = names(rank_model$n.ratings.offense),
    offensive_rating = round(rank_model$n.ratings.offense, 2)
  )) %>% dplyr::arrange(desc(offensive_rating))
  means <- tibble::tribble(
    ~team, ~offensive_rating,
    "Home Mean", round(rank_model$n.mean[["LocationHome"]], 2),
    "Away Mean", round(rank_model$n.mean[["LocationAway"]], 2),
    "Home Edge", round(rank_model$n.mean[["LocationHome"]] -
                         rank_model$n.mean[["LocationAway"]], 2)
  )
  offensive_ratings <- dplyr::bind_rows(offensive_ratings, means)

  defensive_ratings <- tibble::as_tibble(list(
    team = names(rank_model$n.ratings.defense),
    defensive_rating = round(rank_model$n.ratings.defense, 2)
  )) %>% dplyr::arrange(desc(defensive_rating))

  forecast <- game_predict(rank_prep$schedule, rank_model)

  daily_entropy <- forecast %>%
    group_by(date) %>%
    summarize(total_entropy = sum(entropy))

  return(list(
    rank_model = rank_model,
    offensive_ratings = offensive_ratings,
    defensive_ratings = defensive_ratings,
    forecast = forecast,
    daily_entropy = daily_entropy
  ))
}

## global name declarations - See
## https://github.com/STAT545-UBC/Discussion/issues/451#issuecomment-264598618
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
  "defensive_rating",
  "entropy",
  "offensive_rating",
  "binary.response",
  "tov",
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
