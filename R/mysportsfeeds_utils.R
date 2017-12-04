#' @title Install MySportsFeeds Wrapper
#' @name install_mysportsfeeds_wrapper
#' @description Installs the MySportsFeed R wrapper from GitHub
#' @export install_mysportsfeeds_wrapper
#' @return return value from devtools::install_github
#' @examples
#' \dontrun{
#' tidysportsfeeds::install_mysportsfeeds_wrapper()
#' }

install_mysportsfeeds_wrapper <- function() {
  devtools::install_github("znmeb/mysportsfeeds-r")
}

#' @title Get Schedule Gameentry
#' @name get_schedule_gameentry
#' @description Gets a schedule 'gameentry' object from the MySportsFeeds.com API
#' @export get_schedule_gameentry
#' @param league ("nba", "nhl", "nfl" or "mlb")
#' @param season e.g., "2017-2018-regular"
#' @return the gameentry object
#' @examples
#' \dontrun{
#' nba_gameentry <-
#' tidysportsfeeds::get_schedule_gameentry(league = "nba", season = "2017-2018-regular")
#' nhl_gameentry <-
#' tidysportsfeeds::get_schedule_gameentry(league = "nhl", season = "2017-2018-regular")
#' nfl_gameentry <-
#' tidysportsfeeds::get_schedule_gameentry(league = "nfl", season = "2017-regular")
#' }

get_schedule_gameentry <- function(league = "nba", season = "2017-2018-regular") {
  result <- mysportsfeedsR::msf_get_results(
    league = league, season = season, feed = "full_game_schedule", verbose = FALSE)
  gameentry <- tibble::as_tibble(
    result[["api_json"]][["fullgameschedule"]][["gameentry"]])
}
