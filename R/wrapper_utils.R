# See <https://github.com/STAT545-UBC/Discussion/issues/451#issuecomment-264598618>
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
  "teams",
  "players",
  "games",
  "DraftKings",
  "FanDuel"
))

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
  sites <- res[["api_json"]][["dailydfs"]][["dfsEntries"]][["dfsType"]]
  for (i in 1:2) {
    tbl_df <- tibble::as_tibble(
      res[["api_json"]][["dailydfs"]][["dfsEntries"]][["dfsRows"]][[i]]) %>%
      dplyr::mutate(site = sites[i])
    tbl_df$salary <- as.numeric(tbl_df$salary)
    tbl_df$fantasyPoints <- as.numeric(tbl_df$fantasyPoints)
    assign(sites[i], tbl_df)
  }
  return(dplyr::bind_rows(DraftKings, FanDuel))
}

#' @title Get current season tables from Stattleship API
#' @name get_season
#' @description Gets a baseline list of tables for a season from the
#' Stattleship API. Usually, one runs this once at the beginning of the season
#' and updates the tables on a daily or weekly basis.
#' @export get_season
#' @importFrom stattleshipR ss_get_result
#' @param league ("nba", "nhl", "nfl" or "mlb")
#' @return a list of three tibbles:
#' \itemize{
#' \item teams: the teams in the league
#' \item players: the players in the league
#' \item games: games in the season.
#' All games - closed, in-progress and upcaming - are listed.}
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

#' @title Get current season game logs (player box scores) from Stattleship API
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

#' @title Get current season games from Stattleship API
#' @name get_games
#' @description Gets a `games` table from the stattleship.com API
#' @export get_games
#' @importFrom stattleshipR ss_get_result
#' @param league ("nba", "nhl", "nfl" or "mlb")
#' @return a `games table from the Stattleship API. The entire season is
#' returned, with column `status` designating "closed", "upcoming" and
#' "in_progress" games.
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
  return(.get_tibble(
      league = league,
      ep = "games",
      query = list()
    )
  )
}
