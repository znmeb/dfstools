#' @title MySportsFeeds Past NBA Games
#' @name msf_past_nba_games
#' @description Returns a data frame of past NBA games from MySportsFeeds
#' @importFrom dplyr bind_rows
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @importFrom mysportsfeedsR msf_get_results
#' @export msf_past_nba_games
#' @param seasons vector of season codes
#' @return a tibble of completed NBA games
#' @examples
#' \dontrun{
#' apikey <- "your_API key"
#' library(dfstools)
#' library(mysportsfeedsR)
#' authenticate_v2_x(apikey)
#' seasons <- c("2017-2018-regular", "2018-playoff")
#' nba_games <- msf_past_nba_games(seasons)
#' }

msf_past_nba_games <- function(seasons) {
  games <- tibble::tibble()
  for (ixseason in seasons) {
    result <- mysportsfeedsR::msf_get_results(
      version = "2.0",
      league = "nba",
      season = ixseason,
      feed = "seasonal_games",
      verbose = TRUE)
    game_list <- result[["api_json"]][["games"]] %>%
      tibble::as_tibble() %>%
      dplyr::mutate(season = ixseason)
    games <- dplyr::bind_rows(games, game_list)
  }

  return(games)
}

#' @title Get MySportsFeed DFS data
#' @name get_mysportsfeeds_dfs
#' @description Gets DFS data object from the MySportsFeeds.com API
#' @export get_mysportsfeeds_dfs
#' @importFrom mysportsfeedsR msf_get_results
#' @importFrom tibble as_tibble
#' @param league ("nba", "nhl", "nfl" or "mlb")
#' @param season Look up season code in API docs https://www.mysportsfeeds.com/data-feeds/api-docs
#' @return a list of tibbles with the DFS data for the league and season. There is one tibble for each DFS site, currently "DraftKings" and "FanDuel".
#' @examples
#' \dontrun{
#' username <- "your_user_name"
#' password <- "your_password"
#' library(dfstools)
#' library(mysportsfeedsR)
#' authenticate_v1_x(username, password)
#' nba_dfs_2017_2018 <- get_mysportsfeeds_dfs(
#'   league = "nba", season = "2017-2018-regular")
#' nhl_dfs_2017_2018 <- get_mysportsfeeds_dfs(
#'   league = "nhl", season = "2017-2018-regular")
#' mlb_dfs_2017 <- get_mysportsfeeds_dfs(
#'   league = "mlb", season = "2017-regular")
#' nfl_dfs_2017 <- get_mysportsfeeds_dfs(
#'   league = "nfl", season = "2017-regular")
#' }

get_mysportsfeeds_dfs <- function(league, season) {
  res <- mysportsfeedsR::msf_get_results(
    version = "1.2",
    league = league,
    season = season,
    feed = "daily_dfs",
    verbose = FALSE)
  sites <-
    res[["api_json"]][["dailydfs"]][["dfsEntries"]][["dfsType"]]
  return_list <- list()
  for (i in 1:length(sites)) {
    tbl_df <- tibble::as_tibble(
      res$api_json$dailydfs$dfsEntries$dfsRows[[i]])
    tbl_df$salary <- as.numeric(tbl_df$salary)
    tbl_df$fantasyPoints <- as.numeric(tbl_df$fantasyPoints)
    return_list[[sites[i]]] <- tbl_df
  }
  return(return_list)
}
