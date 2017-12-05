#' @title Install MySportsFeeds and Stattleship Wrappers
#' @name install_api_wrappers
#' @description Installs the MySportsFeed and Stattleship R wrappers from GitHub
#' @export install_api_wrappers
#' @return list of return values from the wrapper installs
#' @examples
#' \dontrun{
#' install_status <- tidysportsfeeds::install_api_wrappers()
#' }

install_api_wrappers <- function() {
  list(
    mysportsfeeds =
      devtools::install_github("znmeb/mysportsfeeds-r", force = TRUE),
    stattleship =
      devtools::install_github("znmeb/stattleship-r", force = TRUE))
}

#' @title Get Stattleship Games
#' @name get_stattleship_games
#' @description Gets a schedule 'gameentry' object from the MySportsFeeds.com API
#' @export get_stattleship_games
#' @import stattleshipR
#' @param league ("nba", "nhl", "nfl" or "mlb")
#' @param sport e.g., "basketball"
#' @return a tibble with the games for the season. The whole schedule is given,
#' with games as yet unplayed having scores of zero.
#' @examples
#' \dontrun{
#' token <- "yourtoken"
#' library(tidysportsfeeds)
#' library(stattleshipR)
#' stattleshipR::set_token(token)
#' nba_stattleship_games <-
#'   tidysportsfeeds::get_stattleship_games(league = "nba", sport = "basketball")
#' nhl_stattleship_games <-
#'   tidysportsfeeds::get_stattleship_games(league = "nhl", sport = "hockey")
#' nfl_stattleship_games <-
#'   tidysportsfeeds::get_stattleship_games(league = "nfl", sport = "football")
#' }

get_stattleship_games <- function(
  league = "nba", sport = "basketball") {
  result <- stattleshipR::ss_get_result(
    league = league,
    sport = sport,
    ep = "games",
    walk = TRUE,
    verbose = FALSE)
  stattleship_games <- tibble::as_tibble(
    do.call("rbind", lapply(result, function(x) x$games)))
}
