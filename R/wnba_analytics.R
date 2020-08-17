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
