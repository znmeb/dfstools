# internal function to look up sport name
.sport <- function(league) {
  sports <- c("basketball", "hockey", "football", "baseball")
  names(sports) <- c("nba", "nhl", "nfl", "mlb")
  return(sports[league])
}

# internal function to fetch from Stattleship and unpack the main result as a tibble
.get_tibble <- function(league, ep, query) {
  result <- stattleshipR::ss_get_result(
    sport = .sport(league),
    league = league,
    ep = ep,
    query = query,
    walk = TRUE,
    verbose = FALSE)

  return(tibble::as_tibble(
    do.call("rbind", lapply(result, function(x) get(ep, x)))))
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

#' @title Get Current Season tables from Stattleship API
#' @name get_season
#' @description Gets a schedule 'gameentry' object from the stattleship.com API
#' @export get_season
#' @importFrom stattleshipR ss_get_result
#' @param league ("nba", "nhl", "nfl" or "mlb")
#' @return a list of three tibbles:
#' \itemize{
#' \item teams: the teams in the league
#' \item players: the players in the league
#' \item games: games in the season.
#' All games - completed, in-progress and upcaming - are listed.}
#' @examples
#' \dontrun{
#' token <- "yourtoken"
#' library(tidysportsfeeds)
#' library(stattleshipR)
#' stattleshipR::set_token(token)
#' nba_season <-
#'   tidysportsfeeds::get_season(league = "nba")
#' nhl_season <-
#'   tidysportsfeeds::get_season(league = "nhl")
#' nfl_season <-
#'   tidysportsfeeds::get_season(league = "nfl")
#' }

get_season <- function(league) {
  for (table in c("teams", "players", "games")) {
    assign(table, .get_tibble(
      league = league,
      ep = table,
      query = list()
    ))
  }
  return(list(teams = teams, players = players, games = games))
}

## See <https://github.com/STAT545-UBC/Discussion/issues/451#issuecomment-264598618>
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
  "teams",
  "players",
  "games"
))

#' @title Get Current Season (player) game logs from Stattleship API
#' @name get_game_logs
#' @description Gets a game_logs object from the stattleship.com API
#' @export get_game_logs
#' @importFrom stattleshipR ss_get_result
#' @param league ("nba", "nhl", "nfl" or "mlb")
#' @param team_slug the Stattleship team slug for the team
#' @return a tibble with the game logs
#' @examples
#' \dontrun{
#' token <- "yourtoken"
#' library(tidysportsfeeds)
#' library(stattleshipR)
#' stattleshipR::set_token(token)
#' trailblazers_logs <-
#'   tidysportsfeeds::get_game_logs(league = "nba", team_slug = "nba-por")
#' }

get_game_logs <- function(league, team_slug) {
  return(
    .get_tibble(
      league = league,
      ep = "game_logs",
      query = list(team_id = team_slug)
    )
  )
}

## See <https://github.com/STAT545-UBC/Discussion/issues/451#issuecomment-264598618>
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
  "teams",
  "players",
  "games"
))
