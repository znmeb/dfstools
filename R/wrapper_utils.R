#' @title MySportsFeeds Past NBA Games
#' @name msf_past_nba_games
#' @description Returns a data frame of past NBA games from MySportsFeeds version 2.0 API
#' @importFrom dplyr bind_rows
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom tibble tibble
#' @importFrom httr GET
#' @importFrom httr authenticate
#' @importFrom httr status_code
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate with_tz
#' @importFrom lubridate as_datetime
#' @importFrom lubridate as_date
#' @export msf_past_nba_games
#' @param seasons vector of season codes
#' @param apikey your MySportsFeeds API key (version 2.0!)
#' @return a tibble of completed NBA games
#' @details `msf_past_nba_games` adds two columns at the right of the tibble:
#'   `season`, the source season of the row, and `date`, the game
#'   date (started) in the Eastern USA timezone ("EST5EDT"). The returned
#'   tibble will be sorted in chronological order.
#' @examples
#' \dontrun{
#' apikey <- "your_API key"
#' library(dfstools)
#' seasons <- c(
#'   "2018-playoff",
#'   "2017-2018-regular",
#'   "2017-playoff",
#'   "2016-2017-regular",
#'   "2016-playoff",
#'   "2015-2016-regular")
#' nba_games <- msf_past_nba_games(seasons, apikey)
#' }

msf_past_nba_games <- function(seasons, apikey) {
  games <- tibble::tibble()
  for (ixseason in seasons) {
    ixurl <- paste(
      "https://api.mysportsfeeds.com/v2.0/pull/nba",
      ixseason,
      "games.json",
      sep = "/"
    )
    response <- httr::GET(
      url = ixurl,
      httr::authenticate(apikey, "MYSPORTSFEEDS")
    )
    status_code = httr::status_code(response)
    if (status_code != 200) {
      print(response)
      stop(paste("status_code =", status_code))
    }
    json <- response %>% httr::content(as = "text")
    game_list <- jsonlite::fromJSON(json, flatten = TRUE)[["games"]] %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        season = ixseason,
        date = lubridate::as_datetime(schedule.startTime) %>%
          lubridate::with_tz("EST5EDT") %>%
          strftime(., format = "%Y%m%d")
      )
    games <- dplyr::bind_rows(games, game_list)
  }
  return(games %>% dplyr::arrange(schedule.startTime))
}

#' @title MySportsFeeds Past NBA DFS
#' @name msf_past_nba_dfs
#' @description Gets DFS data object from from MySportsFeeds version 2.0 API
#' @export msf_past_nba_dfs
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom httr GET
#' @importFrom httr authenticate
#' @importFrom httr status_code
#' @importFrom jsonlite fromJSON
#' @param nba_games a tibble produced by `msf_past_nba_games`
#' @param apikey your MySportsFeeds API key (version 2.0!)
#' @return a tibble with all the DFS data available for the input games
#' @examples
#' \dontrun{
#' apikey <- "your_API key"
#' library(dfstools)
#' seasons <- c(
#'   "2018-playoff",
#'   "2017-2018-regular",
#'   "2017-playoff",
#'   "2016-2017-regular",
#'   "2016-playoff",
#'   "2015-2016-regular")
#' nba_games <- msf_past_nba_games(seasons, apikey)
#' nba_dfs <- msf_past_nba_dfs(nba_games, apikey)
#' }

msf_past_nba_dfs <- function(nba_games, apikey) {

  # there are fewer dates than games, so we hit the API by date rather than by game
  dates <- nba_games %>%
    dplyr::select(date, season) %>%
    dplyr::arrange(date) %>%
    dplyr::distinct(date, .keep_all = TRUE)
  nba_dfs <- tibble::tibble()
  for (ixrow in 1:nrow(dates)) {
    ixdate <- dates$date[ixrow]
    ixseason <- dates$season[ixrow]
    ixurl <- paste(
      "https://api.mysportsfeeds.com/v2.0/pull/nba",
      ixseason,
      "date",
      ixdate,
      "dfs.json",
      sep = "/"
    )
    print(ixurl)
    Sys.sleep(6)
    response <- httr::GET(
      url = ixurl,
      httr::authenticate(apikey, "MYSPORTSFEEDS")
    )
    status_code = httr::status_code(response)
    if (status_code != 200) {
      print(response)
      stop(paste("status_code =", status_code))
    }
    json <- response %>% httr::content(as = "text")
    list <- jsonlite::fromJSON(json, flatten = TRUE)
    if (length(list[["dfsEntries"]]) == 0) {
      print("No data - skipping")
      next
    }
    sites <- list[["dfsEntries"]][["dfsSource"]]
    frames <- list[["dfsEntries"]][["dfsRows"]]
    for (ixsite in 1:length(sites)) {
      site <- sites[ixsite]
      frame <- tibble::as_tibble(frames[[ixsite]]) %>%
        dplyr::mutate(dfs_site = site)
      nba_dfs <- dplyr::bind_rows(nba_dfs, frame)
    }
  }
  return(nba_dfs)
}

if(getRversion() >= "2.15.1") utils::globalVariables(c(
  ".",
  "schedule.startTime",
  "startEastern",
  "season"
))
