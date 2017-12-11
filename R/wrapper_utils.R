# internal function to look up sport name
.sport <- function(league) {
  sports <- c("basketball", "hockey", "football", "baseball")
  names(sports) <- c("nba", "nhl", "nfl", "mlb")
  return(sports[league])
}

#' @title Get MySportsFeed DFS data
#' @name get_mysportsfeeds_dfs
#' @description Gets DFS data object from the MySportsFeeds.com API
#' @export get_mysportsfeeds_dfs
#' @importFrom mysportsfeedsR msf_get_results
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @param league ("nba", "nhl", "nfl" or "mlb")
#' @param season Look up season code in API docs https://www.mysportsfeeds.com/data-feeds/api-docs
#' @return a tibble with the DFS data for the league and season.
#' @examples
#' \dontrun{
#' username <- "your_user_name"
#' password <- "your_password"
#' library(tidysportsfeeds)
#' library(mysportsfeedsR)
#' mysportsfeedsR::authenticate_v1_0(username, password)
#' nba_dfs_2017_2018 <-
#'   tidysportsfeeds::get_mysportsfeeds_dfs(league = "nba", season = "2017-2018-regular")
#' }

get_mysportsfeeds_dfs <- function(league, season) {
  res <- mysportsfeedsR::msf_get_results(
    league = league,
    season = season,
    feed = "daily_dfs",
    verbose = FALSE)
  DraftKings <- tibble::as_tibble(
    res[["api_json"]][["dailydfs"]][["dfsEntries"]][["dfsRows"]][[1]]) %>%
    dplyr::mutate(site = "DraftKings")
  FanDuel <- tibble::as_tibble(
    res[["api_json"]][["dailydfs"]][["dfsEntries"]][["dfsRows"]][[2]]) %>%
    dplyr::mutate(site = "FanDuel")
  return(dplyr::bind_rows(DraftKings, FanDuel))
}

#' @title Get Current Season Games from Stattleship API
#' @name get_games
#' @description Gets a schedule 'gameentry' object from the stattleship.com API
#' @export get_games
#' @importFrom stattleshipR ss_get_result
#' @param league ("nba", "nhl", "nfl" or "mlb")
#' @return a tibble with the games for the season. The whole schedule is given,
#' with games as yet unplayed having scores of zero.
#' @examples
#' \dontrun{
#' token <- "yourtoken"
#' library(tidysportsfeeds)
#' library(stattleshipR)
#' stattleshipR::set_token(token)
#' nba_games <-
#'   tidysportsfeeds::get_games(league = "nba")
#' nhl_games <-
#'   tidysportsfeeds::get_games(league = "nhl")
#' nfl_games <-
#'   tidysportsfeeds::get_games(league = "nfl")
#' }

get_games <- function(league) {
  result <- stattleshipR::ss_get_result(
    league = league,
    sport = .sport(league),
    ep = "games",
    walk = TRUE,
    verbose = FALSE)
  return(tibble::as_tibble(
    do.call("rbind", lapply(result, function(x) x$games))))
}

#' @title Get Current Season Teams from Stattleship API
#' @name get_teams
#' @description Gets the teams for a league's current season from the stattleship.com API
#' @export get_teams
#' @importFrom stattleshipR ss_get_result
#' @param league ("nba", "nhl", "nfl" or "mlb")
#' @return a tibble with the teams for the season.
#' @examples
#' \dontrun{
#' token <- "yourtoken"
#' library(tidysportsfeeds)
#' library(stattleshipR)
#' stattleshipR::set_token(token)
#' nba_teams <-
#'   tidysportsfeeds::get_teams(league = "nba")
#' nhl_teams <-
#'   tidysportsfeeds::get_teams(league = "nhl")
#' nfl_teams <-
#'   tidysportsfeeds::get_teams(league = "nfl")
#' }

get_teams <- function(league) {
  result <- stattleshipR::ss_get_result(
    league = league,
    sport = .sport(league),
    ep = "teams",
    walk = TRUE,
    verbose = FALSE)
  return(tibble::as_tibble(
    do.call("rbind", lapply(result, function(x) x$teams))))
}

#' @title Get Current Season PLayers from Stattleship API
#' @name get_players
#' @description Gets the players for a league's current season from the stattleship.com API
#' @export get_players
#' @importFrom stattleshipR ss_get_result
#' @param league ("nba", "nhl", "nfl" or "mlb")
#' @return a tibble with the players for the season.
#' @examples
#' \dontrun{
#' token <- "yourtoken"
#' library(tidysportsfeeds)
#' library(stattleshipR)
#' stattleshipR::set_token(token)
#' nba_players <-
#'   tidysportsfeeds::get_players(league = "nba")
#' nhl_players <-
#'   tidysportsfeeds::get_players(league = "nhl")
#' nfl_players <-
#'   tidysportsfeeds::get_players(league = "nfl")
#' }

get_players <- function(league) {
  result <- stattleshipR::ss_get_result(
    league = league,
    sport = .sport(league),
    ep = "players",
    walk = TRUE,
    verbose = FALSE)
  return(tibble::as_tibble(
    do.call("rbind", lapply(result, function(x) x$players))))
}

#' @title Get Current Season PLayers from Stattleship API
#' @name get_game_logs
#' @description Gets the game_logs for a league's current season from the stattleship.com API
#' @export get_game_logs
#' @importFrom stattleshipR ss_get_result
#' @param league ("nba", "nhl", "nfl" or "mlb")
#' @return a tibble with the game_logs for the season.
#' @examples
#' \dontrun{
#' token <- "yourtoken"
#' library(tidysportsfeeds)
#' library(stattleshipR)
#' stattleshipR::set_token(token)
#' nba_game_logs <-
#'   tidysportsfeeds::get_game_logs(league = "nba")
#' nhl_game_logs <-
#'   tidysportsfeeds::get_game_logs(league = "nhl")
#' nfl_game_logs <-
#'   tidysportsfeeds::get_game_logs(league = "nfl")
#' }

get_game_logs <- function(league) {
  result <- stattleshipR::ss_get_result(
    league = league,
    sport = .sport(league),
    ep = "game_logs",
    walk = TRUE,
    verbose = FALSE)
  #return(tibble::as_tibble(
    #do.call("rbind", lapply(result, function(x) x$game_logs))))
}
