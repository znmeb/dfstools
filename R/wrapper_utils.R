# See <https://github.com/STAT545-UBC/Discussion/issues/451#issuecomment-264598618>
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
  "teams",
  "players",
  "games",
  "DraftKings",
  "FanDuel",
  "game_played",
  "home_team_outcome",
  "time_played_total",
  "birth_date",
  "height",
  "unit_of_height",
  "id",
  "name",
  "position_abbreviation",
  "weight",
  "unit_of_weight",
  "years_of_experience",
  "assists",
  "is_home_team",
  "scoreline",
  "triple_double",
  "double_double",
  "name.y",
  "rebounds_defensive"

))

# names of teams for endpoints / queries
.team_slugs <- list()
.team_slugs[["nba"]] <- c(
  "nba-atl", "nba-bos", "nba-bk", "nba-cha", "nba-chi",
  "nba-cle", "nba-dal", "nba-den", "nba-det", "nba-gs",
  "nba-hou", "nba-ind", "nba-lac", "nba-lal", "nba-mem",
  "nba-mia", "nba-mil", "nba-min", "nba-no", "nba-ny",
  "nba-okc", "nba-orl", "nba-phi", "nba-pho", "nba-por",
  "nba-sac", "nba-sa", "nba-tor", "nba-uta", "nba-was")

.team_slugs[["nfl"]] <- c(
  "nfl-ari", "nfl-atl", "nfl-bal", "nfl-buf", "nfl-car",
  "nfl-chi", "nfl-cin", "nfl-cle", "nfl-dal", "nfl-den",
  "nfl-det", "nfl-gb", "nfl-hou", "nfl-ind", "nfl-jac",
  "nfl-kc", "nfl-lac", "nfl-stl", "nfl-mia", "nfl-min",
  "nfl-ne", "nfl-no", "nfl-nyj", "nfl-nyg", "nfl-oak",
  "nfl-phi", "nfl-pit", "nfl-sf", "nfl-sea", "nfl-tb",
  "nfl-ten", "nfl-was")

.team_slugs[["nhl"]] <- c(
  "nhl-ana", "nhl-ari", "nhl-bos", "nhl-buf", "nhl-cal",
  "nhl-car", "nhl-chi", "nhl-col", "nhl-clb", "nhl-dal",
  "nhl-det", "nhl-edm", "nhl-fla", "nhl-la", "nhl-min",
  "nhl-mon", "nhl-nas", "nhl-nj", "nhl-nyi", "nhl-nyr",
  "nhl-ott", "nhl-phi", "nhl-pit", "nhl-sj", "nhl-stl",
  "nhl-tb", "nhl-tor", "nhl-van", "nhl-vgk", "nhl-was",
  "nhl-win")

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


# internal function to fetch from Stattleship and unpack the main result as a list of tibbles
.get_list_of_tibbles <- function(league, ep, query, tibbles) {
  result <- stattleshipR::ss_get_result(
    sport = .sport(league),
    league = league,
    ep = ep,
    query = query,
    walk = TRUE,
    verbose = FALSE)
  return_list <- list()
  for (t in tibbles) {
    return_list[[t]] <- tibble::as_tibble(
      do.call("rbind", lapply(result, function(x) get(t, x))))
  }
  return(return_list)
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
#'   get_mysportsfeeds_dfs(league = "nba", season = "2017-2018-regular")
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
#'   get_season(league = "nba")
#' nhl_season <-
#'   get_season(league = "nhl")
#' nfl_season <-
#'   get_season(league = "nfl")
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
#'   get_game_logs(league = "nba", team_slug = "nba-por")
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
#'   get_games(league = "nba")
#' nhl_games <-
#'   get_games(league = "nhl")
#' nfl_games <-
#'   get_games(league = "nfl")
#' }

get_games <- function(league) {
  return(.get_tibble(
      league = league,
      ep = "games",
      query = list()
    )
  )
}

#' @title Get current season game logs to date from Stattleship API
#' @name get_game_logs_to_date
#' @description Gets game logs to date for all teams from the stattleship.com API
#' @export get_game_logs_to_date
#' @importFrom stattleshipR ss_get_result
#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#' @importFrom tibble as_tibble
#' @param league ("nba", "nhl" -- "nfl", "mlb" TBD!)
#' @return a list with three items:
#' \itemize{
#' \item players the players tibble returned by the API
#' \item games the games tibble returned by the API
#' \item game_logs the game logs (player box scores) as delivered by the API}
#' @examples
#' \dontrun{
#' token <- "yourtoken"
#' library(tidysportsfeeds)
#' stattleshipR::set_token(token)
#' nba_data <- get_game_logs_to_date(league = "nba")
#' nhl_data <- get_game_logs_to_date(league = "nhl")
#' }

get_game_logs_to_date <- function(league) {
  game_logs <- games <- players <- tibble::as_tibble() # empty tibble
  team_slugs <- .team_slugs[[league]]
  for (i in 1:length(team_slugs)) {
    slug <- team_slugs[i]
    cat("Fetching", slug, "\n")
    tibbles <- .get_list_of_tibbles(
      league = league,
      ep = "game_logs",
      query = list(team_id = slug),
      tibbles = c("players", "games", "game_logs")
    )
    players %<>% dplyr::bind_rows(tibbles$players) %>%
      dplyr::distinct(id, .keep_all = TRUE)
    games %<>% dplyr::bind_rows(tibbles$games) %>%
      dplyr::distinct(id, .keep_all = TRUE)
    game_logs %<>% dplyr::bind_rows(tibbles$game_logs) %>%
      dplyr::distinct(id, .keep_all = TRUE)
  }
  return(list(
    game_logs = game_logs,
    players = players,
    games = games))
}

#' @title Get NBA game logs to date, tidied
#' @name get_nba_game_logs
#' @description Gets a tidy game log tibble for the NBA season to date
#' @export get_nba_game_logs
#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @param nba_data a list returned by get_game_logs_to_date for the NBA
#' @return a tidied player game log tibble (player box scores)
#' @examples
#' \dontrun{
#' token <- "yourtoken"
#' library(tidysportsfeeds)
#' stattleshipR::set_token(token)
#' nba_data <- get_game_logs_to_date(league = "nba")
#' nba_game_logs <-
#' get_nba_game_logs(nba_data)
#' }

get_nba_game_logs <- function(nba_data) {
  nba_game_logs <- nba_data$raw_game_logs %>%
    dplyr::left_join(nba_data$games, by = c("game_id" = "id")) %>%
    dplyr::left_join(nba_data$players, by = c("player_id" = "id")) %>%
    dplyr::select(
      name = name.y, birth_date,
      height, unit_of_height, weight, unit_of_weight,
      position_abbreviation, years_of_experience,
      scoreline, started_at, ended_at, period,
      is_home_team,
      assists:time_played_total,
      rebounds_defensive:double_double, triple_double
    )
}
