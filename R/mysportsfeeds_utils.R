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
    mysportsfeeds = devtools::install_github("znmeb/mysportsfeeds-r", force = TRUE),
    stattleship = devtools::install_github("znmeb/stattleship-r", force = TRUE)
  )
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
