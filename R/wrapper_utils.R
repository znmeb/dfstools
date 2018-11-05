#' @title GET from MySportsFeeds API
#' @name get_msf_api
#' @description GETs data from the MySportsFeeds 2.0 API
#' @importFrom httr GET
#' @importFrom httr authenticate
#' @importFrom httr status_code
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @export get_msf_api
#' @param url the URL to GET
#' @param apikey your MySportsFeeds API key (version 2.0!)
#' @return a list of two items
#' \itemize{
#' \item status_code the HTTP status code (200 for success))
#' \item response if status_code == 200, the JSON object; otherwise, the raw text.
#' }
#' @examples
#' \dontrun{
#' apikey <- "your_API key"
#' response <- dfstools::get_msf_api(
#'   "https://api.mysportsfeeds.com/v2.0/pull/nba/2018-playoff/games.json",
#'   apikey
#' )
#' }

get_msf_api <- function(url, apikey) {
  response <- httr::GET(
    url,
    httr::authenticate(apikey, "MYSPORTSFEEDS")
  )
  status_code <- httr::status_code(response)
  if (status_code == 200) {
    return(list(
      status_code = status_code,
      data = jsonlite::fromJSON(
        httr::content(response, as = "text", encoding = "UTF-8"),
        flatten = TRUE
      ))
    )
  } else {
    return(list(
      status_code = status_code,
      data = httr::content(response, as = "text", encoding = "UTF-8")
    ))
  }
}

#' @title MySportsFeeds Seasonal Games
#' @name msf_seasonal_games
#' @description Returns a data frame of games from MySportsFeeds version 2.0 API
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom tibble tibble
#' @importFrom lubridate with_tz
#' @importFrom lubridate as_datetime
#' @export msf_seasonal_games
#' @param league the league to fetch
#' @param season the season to fetch
#' @param apikey your MySportsFeeds API key (version 2.0!)
#' @return a list of two items
#' \itemize{
#' \item status_code the HTTP status code (200 for success))
#' \item response if status_code == 200, a tibble of games; otherwise, the raw text.
#' }
#' @details `msf_seasonal_games` adds four columns at the right of the tibble:
#' \itemize{
#' \item league the source league of the data
#' \item season the source season of the data, and
#' \item date the game date (started) in the Eastern USA timezone ("EST5EDT").
#' \item slug the game slug (date-away_team-home_team)
#' }
#' The returned tibble will be sorted in chronological order.
#' @examples
#' \dontrun{
#' apikey <- "your_API key"
#' nba_games <-
#'   dfstools::msf_seasonal_games(season = "2017-2018-regular", league = "nba", apikey)
#' }

msf_seasonal_games <- function(league, season, apikey) {
  url <- sprintf(
    "https://api.mysportsfeeds.com/v2.0/pull/%s/%s/games.json",
    league,
    season
  )
  response <- get_msf_api(url, apikey)
  status_code <- response[["status_code"]]
  if (status_code != 200) {
    return(response)
  } else {
    games <- response[["data"]][["games"]] %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        league = league,
        season = season,
        date = lubridate::as_datetime(schedule.startTime) %>%
          lubridate::with_tz("EST5EDT") %>%
          strftime(., format = "%Y%m%d"),
        slug = sprintf(
          "%s-%s-%s",
          date,
          schedule.awayTeam.abbreviation,
          schedule.homeTeam.abbreviation
        )
      )
    return(list(
      status_code = status_code,
      games = games %>% dplyr::arrange(schedule.startTime)
    ))
  }
}

#' @title MySportsFeeds Seasonal Team DFS
#' @name msf_seasonal_team_dfs
#' @description Gets DFS data object from from MySportsFeeds version 2.0 API
#' @export msf_seasonal_team_dfs
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @param league the league to fetch
#' @param season the season to fetch
#' @param team the team to fetch
#' @param apikey your MySportsFeeds API key (version 2.0!)
#' @return a list of two items
#' \itemize{
#' \item status_code the HTTP status code (200 for success, -200 for HTTP success but no DFS data))
#' \item response if status_code == 200, a tibble of DFS data; otherwise, the raw text.
#' }
#' @examples
#' \dontrun{
#' apikey <- "your_API key"
#' nba_dfs <- dfstools::msf_seasonal_team_dfs(
#'   season = "2018-playoff",
#'   league = "nba",
#'   team = "GSW",
#'   apikey)
#' }

msf_seasonal_team_dfs <- function(season, league, team, apikey) {
  url <- sprintf(
    "https://api.mysportsfeeds.com/v2.0/pull/%s/%s/dfs.json?team=%s",
    league,
    season,
    team
  )
  response <- get_msf_api(url, apikey)
  status_code <- response[["status_code"]]
  if (status_code != 200) {
    return(response)
  } else {
    if (length(response[["data"]][["dfsEntries"]]) == 0) {
      return(list(-200, response))
    }
    sites <- response[["data"]][["dfsEntries"]][["dfsSource"]]
    frames <- response[["data"]][["dfsEntries"]][["dfsRows"]]
    dfs <- tibble::tibble()
    for (ixsite in 1:length(sites)) {
      site <- sites[ixsite]
      frame <- tibble::as_tibble(frames[[ixsite]]) %>%
        dplyr::mutate(dfs_site = site)
      dfs <- dplyr::bind_rows(dfs, frame)
    }
    return(list(
      status_code = 200,
      dfs = dfs
    ))
  }
}


#' @title MySportsFeeds Seasonal Player Gamelogs
#' @name msf_seasonal_player_gamelogs
#' @description Gets gamelogs object from from MySportsFeeds version 2.0 API
#' @export msf_seasonal_player_gamelogs
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @param league the league to fetch
#' @param season the season to fetch
#' @param team the team to fetch
#' @param apikey your MySportsFeeds API key (version 2.0!)
#' @return a list of two items
#' \itemize{
#' \item status_code the HTTP status code (200 for success, -200 for HTTP success but no DFS data))
#' \item response if status_code == 200, a tibble of DFS data; otherwise, the raw text.
#' }
#' @examples
#' \dontrun{
#' apikey <- "your_API key"
#' nba_gamelogs <- dfstools::msf_seasonal_player_gamelogs(
#'   season = "2018-playoff",
#'   league = "nba",
#'   team = "GSW",
#'   apikey)
#' }

msf_seasonal_player_gamelogs <- function(season, league, team, apikey) {
  url <- sprintf(
    "https://api.mysportsfeeds.com/v2.0/pull/%s/%s/player_gamelogs.json?team=%s",
    league,
    season,
    team
  )
  response <- get_msf_api(url, apikey)
  status_code <- response[["status_code"]]
  if (status_code != 200) {
    return(response)
  } else {

    if (length(response[["data"]][["gamelogs"]]) == 0) {
      return(list(-200, response))
    }

    gamelogs <- response[["data"]][["gamelogs"]]
    return(list(
      status_code = 200,
      gamelogs = gamelogs
    ))
  }
}

utils::globalVariables(c(
  ".",
  "schedule.attendance",
  "schedule.awayTeam.abbreviation",
  "schedule.delayedOrPostponedReason",
  "schedule.endedTime",
  "schedule.homeTeam.abbreviation",
  "schedule.originalStartTime",
  "schedule.startTime",
  "schedule.weather",
  "score.currentIntermission",
  "score.currentQuarter",
  "score.quarters"
))
