#' @title Set MySportsFeeds API key
#' @name set_msf_apikey
#' @description sets the MySportsFeeds API key into the keyring
#' @importFrom keyring key_set_with_value
#' @export set_msf_apikey
#' @param apikey the API key
#' @details `dfstools` uses the `keyring` package
#' to manage the MySportsFeeds API key. This is portable; it
#' will work on any platform where `keyring`` can be
#' installed, including Linux, MacOS and Windows.
#'
#' Usage:
#' \enumerate{
#'   \item Go to the MySportsFeeds Patreon page
#'   \url{https://www.patreon.com/mysportsfeeds/posts} and become
#'   a patron. You'll need to be a patron to use the v2.0 API.
#'   \item Go to the MySportsFeeds home page
#'   \url{https://www.mysportsfeeds.com/} and sign up.
#'   \item Create a v2.0 API key and copy it to the clipboard.
#'   This package does \strong{not} support the v1.x APIs.
#'   \item In R, paste the API key in the following:
#'
#'     dfstools::set_msf_apikey("paste API key here")
#'  }
#'

set_msf_apikey <- function(apikey) {
  keyring::key_set_with_value("MySportsFeeds", password = apikey)
}

#' @title Get MySportsFeeds API key
#' @name get_msf_apikey
#' @description gets the MySportsFeeds API key saved in the keyring
#' @importFrom keyring key_get
#' @export get_msf_apikey
#' @return the API key

get_msf_apikey <- function() {
  return(keyring::key_get("MySportsFeeds"))
}

#' @title MySportsFeeds Seasons
#' @name msf_seasons
#' @description list the league / season combinations for which
#' MySportsFeeds has DFS data
#' @importFrom tibble tribble
#' @export msf_seasons
#' @return a tibble where each row is a league / season pair
#' @examples
#' \dontrun{
#' seasons <- dfstools::msf_seasons()
#' seasons
#' }

msf_seasons <- function() {
  tibble::tribble(
    ~league, ~season,
    "nba", "2015-2016-regular",
    "nba", "2016-playoff",
    "nba", "2016-2017-regular",
    "nba", "2017-playoff",
    "nba", "2017-2018-regular",
    "nba", "2018-playoff",
    "nhl", "2015-2016-regular",
    "nhl", "2016-playoff",
    "nhl", "2016-2017-regular",
    "nhl", "2017-playoff",
    "nhl", "2017-2018-regular",
    "nhl", "2018-playoff",
    "nfl", "2015-regular",
    "nfl", "2016-playoff",
    "nfl", "2016-regular",
    "nfl", "2017-playoff",
    "nfl", "2017-regular",
    "nfl", "2018-playoff",
    "mlb", "2016-regular",
    "mlb", "2016-playoff",
    "mlb", "2017-regular",
    "mlb", "2017-playoff",
    "mlb", "2018-playoff",
    "mlb", "2018-playoff"
  )
}

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
#' @param verbose print status info
#' @return a list of two items
#' \itemize{
#'   \item status_code the HTTP status code (200 for success)
#'   \item response if status_code == 200, the JSON object;
#'     otherwise, the raw text.
#' }
#' @examples
#' \dontrun{
#' response <- dfstools::get_msf_api(
#' "https://api.mysportsfeeds.com/v2.0/pull/nba/2018-playoff/games.json"
#' )}

get_msf_api <- function(url, verbose = FALSE) {

  # you shouldn't have to change these!
  tries <- 5
  sleep_seconds <- 10

  apikey <- dfstools::get_msf_apikey()
  for (ixtry in 1:tries) {
    if (verbose) print(url)
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
        )
      ))
    } else {
      if (verbose) {
        print(paste(
          status_code, "sleeping", sleep_seconds
        ))
      }
      Sys.sleep(sleep_seconds)
    }
  }
  return(list(
    status_code = status_code,
    data = httr::content(response, as = "text", encoding = "UTF-8")
  ))
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
#' @param verbose print status info
#' @return a list of two items
#' \itemize{
#'   \item status_code the HTTP status code (200 for success)
#'   \item response if status_code == 200, a tibble of games;
#'   otherwise, the raw text.
#' }
#' @details `msf_seasonal_games` adds four columns at the right of the tibble:
#' \itemize{
#'   \item league the source league of the data
#'   \item season the source season of the data, and
#'   \item date the game date (started) in the Eastern USA
#'     timezone ("EST5EDT").
#'   \item slug the game slug (date-away_team-home_team)
#' }
#'
#' The returned tibble will be sorted in chronological order.
#' @examples
#' \dontrun{
#' nba_games <- dfstools::msf_seasonal_games(
#' season = "2017-2018-regular", league = "nba"
#' )}

msf_seasonal_games <- function(league, season, verbose = FALSE) {
  url <- sprintf(
    "https://api.mysportsfeeds.com/v2.0/pull/%s/%s/games.json",
    league,
    season
  )
  response <- get_msf_api(url, verbose = verbose)
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

#' @title MySportsFeeds Seasonal Players
#' @name msf_seasonal_players
#' @description Returns a data frame of players from
#' MySportsFeeds version 2.0 API
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom tibble tibble
#' @importFrom lubridate with_tz
#' @importFrom lubridate as_datetime
#' @export msf_seasonal_players
#' @param league the league to fetch
#' @param season the season to fetch
#' @param verbose print status info
#' @return a list of two items
#' \itemize{
#'   \item status_code the HTTP status code (200 for success)
#'   \item response if status_code == 200, a tibble of players;
#'   otherwise, the raw text.
#' }
#' @examples
#' \dontrun{
#' nba_players <- dfstools::msf_seasonal_players(
#' season = "2017-2018-regular", league = "nba"
#' )}

msf_seasonal_players <- function(league, season, verbose = FALSE) {
  url <- sprintf(
    "https://api.mysportsfeeds.com/v2.0/pull/%s/players.json?season=%s",
    league,
    season
  )
  response <- get_msf_api(url, verbose = verbose)
  status_code <- response[["status_code"]]
  if (status_code != 200) {
    return(response)
  } else {
    players <- response[["data"]][["players"]] %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        league = league,
        season = season)
    return(list(
      status_code = status_code,
      players = players
    ))
  }
}

#' @title MySportsFeeds Seasonal DFS
#' @name msf_seasonal_dfs
#' @description Gets DFS data object from from MySportsFeeds version 2.0 API
#' @export msf_seasonal_dfs
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @param league the league to fetch
#' @param season the season to fetch
#' @param team the team to fetch
#' @param verbose print status info
#' @return a list of two items
#' \itemize{
#'   \item status_code the HTTP status code (200 for success,
#'     -200 for HTTP success but no DFS data)
#'   \item response if status_code == 200, a tibble of DFS data;
#'     otherwise, the raw response text.
#' }
#' @examples
#' \dontrun{
#' nba_dfs <- dfstools::msf_seasonal_dfs(
#' season = "2018-playoff", league = "nba", team = "GSW"
#' )}

msf_seasonal_dfs <- function(
  season, league, team, verbose = FALSE) {
  url <- sprintf(
    "https://api.mysportsfeeds.com/v2.0/pull/%s/%s/dfs.json?team=%s",
    league,
    season,
    team
  )
  response <- get_msf_api(url, verbose = verbose)
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
#' @description Gets player gamelogs object from from
#' MySportsFeeds version 2.0 API
#' @export msf_seasonal_player_gamelogs
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @param league the league to fetch
#' @param season the season to fetch
#' @param team the team to fetch
#' @param verbose print status info
#' @return a list of two items
#' \itemize{
#'   \item status_code the HTTP status code (200 for success,
#'     -200 for HTTP success but no data)
#'   \item  response if status_code == 200, a `player_gamelogs``
#'      object; otherwise, the raw text.
#'  }
#' @examples
#' \dontrun{
#' nba_player_gamelogs <- dfstools::msf_seasonal_player_gamelogs(
#' season = "2018-playoff", league = "nba", team = "GSW"
#' )}

msf_seasonal_player_gamelogs <-
  function(season, league, team, verbose = FALSE) {
    url <- sprintf(
      "https://api.mysportsfeeds.com/v2.0/pull/%s/%s/player_gamelogs.json?team=%s",
      league,
      season,
      team
    )
    response <- get_msf_api(url, verbose = verbose)
    status_code <- response[["status_code"]]
    if (status_code != 200) {
      return(response)
    } else {

      if (length(response[["data"]][["gamelogs"]]) == 0) {
        return(list(-200, response))
      }

      player_gamelogs <- response[["data"]][["gamelogs"]]
      return(list(
        status_code = 200,
        player_gamelogs = player_gamelogs
      ))
    }
  }

#' @title MySportsFeeds Seasonal Team Gamelogs
#' @name msf_seasonal_team_gamelogs
#' @description Gets team gamelogs object from from MySportsFeeds
#' version 2.0 API
#' @export msf_seasonal_team_gamelogs
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @param league the league to fetch
#' @param season the season to fetch
#' @param team the team to fetch
#' @param verbose print status info
#' @return a list of two items
#' \itemize{
#'   \item status_code the HTTP status code (200 for success,
#'     -200 for HTTP success but no DFS data)
#'   \item  response if status_code == 200, a `team_gamelogs``
#'      object; otherwise, the raw text.
#'  }
#' @examples
#' \dontrun{
#' nba_team_gamelogs <- dfstools::msf_seasonal_team_gamelogs(
#' season = "2018-playoff", league = "nba", team = "GSW"
#' )}

msf_seasonal_team_gamelogs <-
  function(season, league, team, verbose = FALSE) {
    url <- sprintf(
      "https://api.mysportsfeeds.com/v2.0/pull/%s/%s/team_gamelogs.json?team=%s",
      league,
      season,
      team
    )
    response <- get_msf_api(url, verbose = verbose)
    status_code <- response[["status_code"]]
    if (status_code != 200) {
      return(response)
    } else {

      if (length(response[["data"]][["gamelogs"]]) == 0) {
        return(list(-200, response))
      }

      team_gamelogs <- response[["data"]][["gamelogs"]]
      return(list(
        status_code = 200,
        team_gamelogs = team_gamelogs
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
