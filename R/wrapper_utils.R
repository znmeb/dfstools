# locals

# We want to ignore two types of columns:
# 1. List columns - databases don't like them, and
# 2. Columns that are filled with NAs - they waste
#    space and time.
.good_column <- function(x) {
  !all(is.na(x)) & (typeof(x) != "list")
}
.good_columns <- function(df) {
  dplyr::select_if(.tbl = df, .predicate = .good_column)
}

#' @title Set MySportsFeeds API key
#' @name msf_set_apikey
#' @description sets the MySportsFeeds API key into the keyring
#' @importFrom keyring key_set_with_value
#' @export msf_set_apikey
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
#'     dfstools::msf_set_apikey("paste API key here")
#'  }
#'

msf_set_apikey <- function(apikey) {
  keyring::key_set_with_value(
    "MySportsFeeds", password = apikey, keyring = "")
}

#' @title Get MySportsFeeds API key
#' @name msf_get_apikey
#' @description gets the MySportsFeeds API key saved in the keyring
#' @importFrom keyring key_get
#' @export msf_get_apikey
#' @return the API key

msf_get_apikey <- function() {
  return(keyring::key_get("MySportsFeeds", keyring = ""))
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
    "nba", "2018-2019-regular",
    "nba", "2019-playoff",
    "nhl", "2015-2016-regular",
    "nhl", "2016-playoff",
    "nhl", "2016-2017-regular",
    "nhl", "2017-playoff",
    "nhl", "2017-2018-regular",
    "nhl", "2018-playoff",
    "nhl", "2018-2019-regular",
    "nhl", "2019-playoff",
    "nfl", "2015-regular",
    "nfl", "2016-playoff",
    "nfl", "2016-regular",
    "nfl", "2017-playoff",
    "nfl", "2017-regular",
    "nfl", "2018-playoff",
    "nfl", "2018-regular",
    "nfl", "2019-playoff",
    "mlb", "2016-regular",
    "mlb", "2016-playoff",
    "mlb", "2017-regular",
    "mlb", "2017-playoff",
    "mlb", "2018-playoff",
    "mlb", "2018-playoff"
  )
}

#' @title GET from MySportsFeeds API
#' @name msf_get_feed
#' @description GETs data from the MySportsFeeds 2.0 API feed
#' @importFrom httr GET
#' @importFrom httr authenticate
#' @importFrom httr status_code
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @export msf_get_feed
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
#' response <- dfstools::msf_get_feed(
#' "https://api.mysportsfeeds.com/v2.0/pull/nba/2018-playoff/games.json"
#' )}

msf_get_feed <- function(url, verbose) {

  # you shouldn't have to change these!
  tries <- 5
  sleep_seconds <- 10

  # only some response codes should be retried
  # see https://www.mysportsfeeds.com/data-feeds/api-docs
  retry_allowed <- c(429, 499, 500, 502, 503)

  apikey <- dfstools::msf_get_apikey()
  for (ixtry in 1:tries) {
    if (verbose) print(url)
    response <- httr::GET(
      url,
      httr::authenticate(apikey, "MYSPORTSFEEDS")
    )
    status_code <- httr::status_code(response)

    # was the GET successful?
    if (status_code == 200) {
      return(list(
        status_code = status_code,
        data = jsonlite::fromJSON(
          httr::content(response, as = "text", encoding = "UTF-8"),
          flatten = TRUE
        )
      ))
    }

    # is it a fatal code?
    if (!(status_code %in% retry_allowed)) {
      return(list(
        status_code = status_code,
        data = httr::content(
          response, as = "text", encoding = "UTF-8"
        )
      ))
    }

    # GET failed but we can retry - sleep and continue retry loop
    if (verbose) {
      print(paste(
        status_code, "sleeping", sleep_seconds
      ))
    }
    Sys.sleep(sleep_seconds)
  }

  # no retry succeeded - return the raw stuff
  return(list(
    status_code = status_code,
    data = httr::content(
      response, as = "text", encoding = "UTF-8")
  ))
}

#' @title MySportsFeeds Seasonal Games
#' @name msf_seasonal_games
#' @description Returns a data frame of games from MySportsFeeds version 2.0 API
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr select_if
#' @importFrom dplyr summarize
#' @importFrom dplyr full_join
#' @importFrom dplyr n
#' @importFrom tibble tibble
#' @importFrom lubridate with_tz
#' @importFrom lubridate as_datetime
#' @importFrom snakecase to_snake_case
#' @importFrom tidyr unnest
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
#'   \item ot number of overtime periods
#' }
#'
#' The returned tibble will be sorted in chronological order.
#' @examples
#' \dontrun{
#' nba_games <- dfstools::msf_seasonal_games(
#' season = "2017-2018-regular", league = "nba"
#' )}

msf_seasonal_games <- function(league, season, verbose) {
  url <- sprintf(
    "https://api.mysportsfeeds.com/v2.0/pull/%s/%s/games.json",
    league,
    season
  )
  response <- msf_get_feed(url, verbose = verbose)
  status_code <- response[["status_code"]]
  if (status_code != 200) {
    return(response)
  } else {
    games <- response[["data"]][["games"]] %>%
      tibble::as_tibble()
    colnames(games) <- colnames(games) %>%
      snakecase::to_snake_case()

    # save the quarter scores for OT calculation
    score_quarters <- games[["score_quarters"]]

    games <- games %>%
      .good_columns() %>%
      dplyr::mutate(
        score_quarters = score_quarters,
        league = league,
        season = season,
        date = lubridate::as_datetime(schedule_start_time) %>%
          lubridate::with_tz("EST5EDT") %>%
          strftime(., format = "%Y%m%d"),
        slug = sprintf(
          "%s-%s-%s",
          date,
          schedule_away_team_abbreviation,
          schedule_home_team_abbreviation
        )
      )
    ot_counts <- games %>% tidyr::unnest() %>%
      dplyr::group_by(slug) %>%
      dplyr::summarize(ot = dplyr::n() - 4)
    games <- dplyr::full_join(games, ot_counts) %>%
      dplyr::select(-score_quarters)
    return(list(
      status_code = status_code,
      games = games %>% dplyr::arrange(schedule_start_time)
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
#' @importFrom dplyr select_if
#' @importFrom tibble tibble
#' @importFrom lubridate with_tz
#' @importFrom lubridate as_datetime
#' @importFrom snakecase to_snake_case
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

msf_seasonal_players <- function(league, season, verbose) {
  url <- sprintf(
    "https://api.mysportsfeeds.com/v2.0/pull/%s/players.json?season=%s",
    league,
    season
  )
  response <- msf_get_feed(url, verbose = verbose)
  status_code <- response[["status_code"]]
  if (status_code != 200) {
    return(response)
  } else {
    players <- response[["data"]][["players"]] %>%
      tibble::as_tibble() %>%
      .good_columns() %>%
      dplyr::mutate(
        league = league,
        season = season)
    colnames(players) <- colnames(players) %>%
      snakecase::to_snake_case()
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
#' @importFrom dplyr select_if
#' @importFrom snakecase to_snake_case
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
  league, season, team, verbose) {
  url <- sprintf(
    "https://api.mysportsfeeds.com/v2.0/pull/%s/%s/dfs.json?team=%s",
    league,
    season,
    team
  )
  response <- msf_get_feed(url, verbose = verbose)
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
    dfs <- dfs %>% .good_columns()
    colnames(dfs) <- colnames(dfs) %>%
      snakecase::to_snake_case()
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
#' @importFrom dplyr select_if
#' @importFrom snakecase to_snake_case
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
  function(league, season, team, verbose) {
    url <- sprintf(
      "https://api.mysportsfeeds.com/v2.0/pull/%s/%s/player_gamelogs.json?team=%s",
      league,
      season,
      team
    )
    response <- msf_get_feed(url, verbose = verbose)
    status_code <- response[["status_code"]]
    if (status_code != 200) {
      return(response)
    } else {

      if (length(response[["data"]][["gamelogs"]]) == 0) {
        return(list(-200, response))
      }

      player_gamelogs <- response[["data"]][["gamelogs"]] %>%
        tibble::as_tibble() %>%
        .good_columns()
      colnames(player_gamelogs) <- colnames(player_gamelogs) %>%
        snakecase::to_snake_case()
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
#' @importFrom dplyr select_if
#' @importFrom snakecase to_snake_case
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
  function(league, season, team, verbose) {
    url <- sprintf(
      "https://api.mysportsfeeds.com/v2.0/pull/%s/%s/team_gamelogs.json?team=%s",
      league,
      season,
      team
    )
    response <- msf_get_feed(url, verbose = verbose)
    status_code <- response[["status_code"]]
    if (status_code != 200) {
      return(response)
    } else {

      if (length(response[["data"]][["gamelogs"]]) == 0) {
        return(list(-200, response))
      }

      team_gamelogs <- response[["data"]][["gamelogs"]] %>%
        tibble::as_tibble() %>%
        .good_columns()
      colnames(team_gamelogs) <- colnames(team_gamelogs) %>%
        snakecase::to_snake_case()
      return(list(
        status_code = 200,
        team_gamelogs = team_gamelogs
      ))
    }
  }

#' @title MySportsFeeds Seasonal Player Stats Totals
#' @name msf_seasonal_player_stats_totals
#' @description Gets player gamelogs object from from
#' MySportsFeeds version 2.0 API
#' @export msf_seasonal_player_stats_totals
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select_if
#' @importFrom snakecase to_snake_case
#' @param league the league to fetch
#' @param season the season to fetch
#' @param verbose print status info
#' @return a list of two items
#' \itemize{
#'   \item status_code the HTTP status code (200 for success,
#'     -200 for HTTP success but no data)
#'   \item  response if status_code == 200, a `player_stats_totals`
#'      object; otherwise, the raw text.
#'  }
#' @examples
#' \dontrun{
#' nba_player_totals <- dfstools::msf_seasonal_player_stats_totals(
#' season = "current", league = "nba", verbose = TRUE
#' )}

msf_seasonal_player_stats_totals <-
  function(league, season, verbose) {
    url <- sprintf(
      "https://api.mysportsfeeds.com/v2.0/pull/%s/%s/player_stats_totals.json",
      league,
      season
    )
    response <- msf_get_feed(url, verbose = verbose)
    status_code <- response[["status_code"]]
    if (status_code != 200) {
      return(response)
    } else {

      if (length(response[["data"]][["playerStatsTotals"]]) == 0) {
        return(list(-200, response))
      }

      player_stats_totals <- response[["data"]][["playerStatsTotals"]] %>%
        tibble::as_tibble() %>%
        .good_columns()
      colnames(player_stats_totals) <- colnames(player_stats_totals) %>%
        snakecase::to_snake_case()
      return(list(
        status_code = 200,
        player_stats_totals = player_stats_totals
      ))
    }
  }

utils::globalVariables(c(
  ".",
  "n",
  "schedule.attendance",
  "schedule_away_team_abbreviation",
  "schedule.delayedOrPostponedReason",
  "schedule.endedTime",
  "schedule_home_team_abbreviation",
  "schedule.originalStartTime",
  "schedule.startTime",
  "schedule_start_time",
  "schedule.weather",
  "score.currentIntermission",
  "score.currentQuarter",
  "score.quarters",
  "slug"
))
