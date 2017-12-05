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

#' @title Get Schedule Gameentry from MySportsFeeds.com
#' @name get_schedule_gameentry
#' @description Gets a schedule 'gameentry' object from the MySportsFeeds.com API
#' @export get_schedule_gameentry
#' @import mysportsfeedsR
#' @param league ("nba", "nhl", "nfl" or "mlb")
#' @param season e.g., "2017-2018-regular"
#' @return the gameentry object
#' @examples
#' \dontrun{
#' username <- "yourusername"
#' password <- "yourpassword"
#' library(tidysportsfeeds)
#' library(mysportsfeedsR)
#' mysportsfeedsR::authenticate_v1_0(username, password)
#' nba_gameentry <-
#'   tidysportsfeeds::get_schedule_gameentry(league = "nba", season = "2017-2018-regular")
#' nhl_gameentry <-
#'   tidysportsfeeds::get_schedule_gameentry(league = "nhl", season = "2017-2018-regular")
#' nfl_gameentry <-
#'   tidysportsfeeds::get_schedule_gameentry(league = "nfl", season = "2017-regular")
#' }

get_schedule_gameentry <- function(
  league = "nba", season = "2017-2018-regular") {
    result <- mysportsfeedsR::msf_get_results(
      league = league,
      season = season,
      feed = "full_game_schedule",
      verbose = FALSE)
    gameentry <- tibble::as_tibble(
      result[["api_json"]][["fullgameschedule"]][["gameentry"]])
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
