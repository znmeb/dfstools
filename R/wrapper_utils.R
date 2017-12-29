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
  DraftKings$salary <- as.numeric(DraftKings$salary)
  DraftKings$fantasyPoints <- as.numeric(DraftKings$fantasyPoints)
  FanDuel <- tibble::as_tibble(
    res[["api_json"]][["dailydfs"]][["dfsEntries"]][["dfsRows"]][[2]]) %>%
    dplyr::mutate(site = "FanDuel")
  FanDuel$salary <- as.numeric(FanDuel$salary)
  FanDuel$fantasyPoints <- as.numeric(FanDuel$fantasyPoints)
  return(dplyr::bind_rows(DraftKings, FanDuel))
}

#' @title Get Current Season Games from Stattleship API
#' @name get_season
#' @description Gets a schedule 'gameentry' object from the stattleship.com API
#' @export get_season
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
#' nba_raw <-
#'   tidysportsfeeds::get_season(league = "nba")
#' nhl_raw <-
#'   tidysportsfeeds::get_season(league = "nhl")
#' nfl_raw <-
#'   tidysportsfeeds::get_season(league = "nfl")
#' }

get_season <- function(league) {
  result <- stattleshipR::ss_get_result(
    league = league,
    sport = .sport(league),
    ep = "games",
    walk = TRUE,
    verbose = FALSE)
  return(tibble::as_tibble(
    do.call("rbind", lapply(result, function(x) x$games))))
}
