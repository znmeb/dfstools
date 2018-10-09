#' @title MySportsFeeds Past NBA Games
#' @name msf_past_nba_games
#' @description Returns a data frame of past NBA games from MySportsFeeds
#' @importFrom dplyr bind_rows
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @importFrom mysportsfeedsR msf_get_results
#' @importFrom lubridate with_tz
#' @importFrom lubridate as_datetime
#' @export msf_past_nba_games
#' @param seasons vector of season codes
#' @return a tibble of completed NBA games
#' @details `msf_past_nba_games` adds two columns at the right of the tibble:
#'   `season`, the source season of the row, and `startEastern`, the game
#'   start time in the Eastern USA timezone ("EST5EDT").
#' @examples
#' \dontrun{
#' apikey <- "your_API key"
#' library(dfstools)
#' library(mysportsfeedsR)
#' authenticate_v2_x(apikey)
#' seasons <- c(
#'   "2018-playoff",
#'   "2017-2018-regular",
#'   "2017-playoff",
#'   "2016-2017-regular",
#'   "2016-playoff",
#'   "2015-2016-regular")
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
      dplyr::mutate(
        season = ixseason,
        startEastern = lubridate::with_tz(
          lubridate::as_datetime(schedule.startTime),
          "EST5EDT"
        )
      )
    games <- dplyr::bind_rows(games, game_list)
  }

  return(games)
}

#' @title MySportsFeeds Past NBA DFS
#' @name msf_past_nba_dfs
#' @description Gets DFS data object from the MySportsFeeds.com API
#' @export msf_past_nba_dfs
#' @importFrom mysportsfeedsR msf_get_results
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr distinct
#' @importFrom utils View
#' @param nba_games a tibble produced with `msf_past_nba_games`
#' @return a tibble with all the DFS data available for the input games
#' @examples
#' \dontrun{
#' apikey <- "your_API key"
#' library(dfstools)
#' library(mysportsfeedsR)
#' authenticate_v2_x(apikey)
#' seasons <- c(
#'   "2018-playoff",
#'   "2017-2018-regular",
#'   "2017-playoff",
#'   "2016-2017-regular",
#'   "2016-playoff",
#'   "2015-2016-regular")
#' nba_games <- msf_past_nba_games(seasons)
#' nba_dfs <- msf_past_nba_dfs(nba_games)
#' }

msf_past_nba_dfs <- function(nba_games) {

  # there are fewer dates than games, so we hit the API by date rather than by game
  dates <- nba_games %>%
    dplyr::mutate(date = as.character(lubridate::as_date(startEastern))) %>%
    dplyr::select(date, season) %>%
    dplyr::arrange(date) %>%
    dplyr::distinct(date, .keep_all = TRUE)
  dfs <- tibble::tibble()
  for (ixrow in 1:nrow(dates)) {
    ixdate <- dates$date[ixrow]
    ixseason <- dates$season[ixrow]
    print(ixseason)
    res <- mysportsfeedsR::msf_get_results(
      version = "2.0",
      league = "nba",
      season = ixseason,
      params = list(date = ixdate),
      feed = "daily_dfs",
      verbose = TRUE)
    View(res)
    stop("testing")
  }
}

if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "schedule.startTime",
  "startEastern",
  "season"
))
