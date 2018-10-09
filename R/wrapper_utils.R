#' @title Get from MySportsFeeds API
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
#' library(dfstools)
#' response <- get_msf_api(
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
        httr::content(response, as = "text"),
        flatten = TRUE
      ))
    )
  } else {
    return(list(
      status_code = status_code,
      data = httr::content(response, as = "text")
    ))
  }
}

#' @title MySportsFeeds NBA Games
#' @name msf_nba_games
#' @description Returns a data frame of NBA games from MySportsFeeds version 2.0 API
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom tibble tibble
#' @importFrom lubridate with_tz
#' @importFrom lubridate as_datetime
#' @export msf_nba_games
#' @param season the season to fetch games for
#' @param apikey your MySportsFeeds API key (version 2.0!)
#' @return a list of two items
#' \itemize{
#' \item status_code the HTTP status code (200 for success))
#' \item response if status_code == 200, a tibble of games; otherwise, the raw text.
#' }
#' @details `msf_nba_games` adds two columns at the right of the tibble:
#'   `season`, the source season of the data, and `date`, the game
#'   date (started) in the Eastern USA timezone ("EST5EDT"). The returned
#'   tibble will be sorted in chronological order.
#' @examples
#' \dontrun{
#' apikey <- "your_API key"
#' library(dfstools)
#' nba_games <- msf_nba_games(season = "2017-2018-regular", apikey)
#' }

msf_nba_games <- function(season, apikey) {
  url <- paste(
    "https://api.mysportsfeeds.com/v2.0/pull/nba",
    season,
    "games.json",
    sep = "/"
  )
  response <- get_msf_api(url, apikey)
  status_code <- response[["status_code"]]
  if (status_code != 200) {
    return(response)
  } else {
    nba_games <- response[["data"]][["games"]] %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        season = season,
        date = lubridate::as_datetime(schedule.startTime) %>%
          lubridate::with_tz("EST5EDT") %>%
          strftime(., format = "%Y%m%d")
      )
    return(list(
      status_code = status_code,
      nba_games = nba_games %>% dplyr::arrange(schedule.startTime)
    ))
  }
}

#' @title MySportsFeeds NBA DFS
#' @name msf_nba_dfs
#' @description Gets DFS data object from from MySportsFeeds version 2.0 API
#' @export msf_nba_dfs
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @param season the season to fetch
#' @param date the date to fetch
#' @param apikey your MySportsFeeds API key (version 2.0!)
#' @return a list of two items
#' \itemize{
#' \item status_code the HTTP status code (200 for success, -200 for success but no DFS data))
#' \item response if status_code == 200, a tibble of DFS data; otherwise, the raw text.
#' }
#' @examples
#' \dontrun{
#' apikey <- "your_API key"
#' library(dfstools)
#' nba_dfs <- msf_nba_dfs("2018-playoff", "20180414", apikey)
#' }

msf_nba_dfs <- function(season, date, apikey) {
  url <- paste(
   "https://api.mysportsfeeds.com/v2.0/pull/nba",
    season,
    "date",
    date,
    "dfs.json",
    sep = "/"
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
    nba_dfs <- tibble::tibble()
    for (ixsite in 1:length(sites)) {
      site <- sites[ixsite]
      frame <- tibble::as_tibble(frames[[ixsite]]) %>%
        dplyr::mutate(dfs_site = site)
      nba_dfs <- dplyr::bind_rows(nba_dfs, frame)
    }
    return(list(
      status_code = 200,
      nba_dfs = nba_dfs
    ))
  }
}

if(getRversion() >= "2.15.1") utils::globalVariables(c(
  ".",
  "schedule.startTime",
  "startEastern",
  "season"
))
