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

# internal function to look up sport name
.sport <- function(league) {
  sports <- c("basketball", "hockey", "football", "baseball")
  names(sports) <- c("nba", "nhl", "nfl", "mlb")
  return(sports[league])
}

#' @title Get Stattleship Games
#' @name get_stattleship_games
#' @description Gets a schedule 'gameentry' object from the MySportsFeeds.com API
#' @export get_stattleship_games
#' @import stattleshipR
#' @param league ("nba", "nhl", "nfl" or "mlb")
#' @return a tibble with the games for the season. The whole schedule is given,
#' with games as yet unplayed having scores of zero.
#' @examples
#' \dontrun{
#' token <- "yourtoken"
#' library(tidysportsfeeds)
#' library(stattleshipR)
#' stattleshipR::set_token(token)
#' nba_stattleship_games <-
#'   tidysportsfeeds::get_stattleship_games(league = "nba")
#' nhl_stattleship_games <-
#'   tidysportsfeeds::get_stattleship_games(league = "nhl")
#' nfl_stattleship_games <-
#'   tidysportsfeeds::get_stattleship_games(league = "nfl")
#' }

get_stattleship_games <- function(league = "nba") {
  result <- stattleshipR::ss_get_result(
    league = league,
    sport = .sport(league),
    ep = "games",
    walk = TRUE,
    verbose = FALSE)
  stattleship_games <- tibble::as_tibble(
    do.call("rbind", lapply(result, function(x) x$games)))
}
