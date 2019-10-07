# locals

# We want to ignore columns that are filled with NAs - they waste space
# and time.
.good_column <- function(x) {
  !all(is.na(x)) & class(x) != "list"
}
.good_columns <- function(df) {
  dplyr::select_if(.tbl = df, .predicate = .good_column)
}

.list_column <- function(league) {
  if (league == "nba" | league == "nfl") return("score_quarters")
  if (league == "mlb") return("score_innings")
  if (league == "nhl") return("score_periods")
  stop(league)
}

.regulation_periods <- function(league) {
  if (league == "nba" | league == "nfl") return(4)
  if (league == "mlb") return(9)
  if (league == "nhl") return(3)
  stop(league)
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
#'   a patron. You'll need to be a patron to use the v2.1 API.
#'   \item Go to the MySportsFeeds home page
#'   \url{https://www.mysportsfeeds.com/} and sign up.
#'   \item Create a v2.1 API key and copy it to the clipboard.
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

#' @title Build MySportsFeeds API URL
#' @name msf_build_url
#' @description builds a MySportsFeeds 2.1 API URL
#' @export msf_build_url
#' @param league c("nba", "nhl", "nfl", "mlb")
#' @param season string the season
#' @param endpoint string the JSON endpoint, for example "games.json"
#' @param team string for feeds that must be subsetted by team (default "")
#' @param date string for feeds that must be subsetted by date (default "")
#' @return the MySportsFeed URL
#' @examples
#' \dontrun{
#' nba_games_url <- dfstools::msf_build_url(
#'   "nba",
#'   "2018-2018-regular",
#'   "games.json"
#' )
#' redwings_gamelogs <- dfstools::msf_build_url(
#'   "nhl",
#'   "2018-2019-regular",
#'   "team_gamelogs.json",
#'   team = "DET"
#' )
#' xmas_nba_games <- msf_build_url(
#'   "nba",
#'   "2019-2020-regular",
#'   "games.json",
#'   date = "20191225"
#' )
#' }

msf_build_url <- function(league, season, endpoint, team = "", date = "") {
  url <- paste(
    "https://api.mysportsfeeds.com/v2.1/pull", league, season, sep = "/"
  )
  if (date != "") {
    url <- paste(url, "date", date, sep = "/")
  }
  url <- paste(url, endpoint, sep = "/")
  if (team != "") {
    url <- paste0(url, "?team=", team)
  }
  return(url)
}

#' @title GET from MySportsFeeds API
#' @name msf_get_feed
#' @description GETs data from the MySportsFeeds 2.1 API feed
#' @importFrom httr GET
#' @importFrom httr authenticate
#' @importFrom httr status_code
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @export msf_get_feed
#' @param url the URL to GET
#' @param verbose print status info (default is TRUE)
#' @return if successful, returns a JSON object as a list
#' if a fatal error, prints the response and does `stop(status_code)`
#' if a recoverable error, retries 4 times with ten-second sleeps,
#' prints the response and does `stop(status_code)` if all retries fail
#'
#' @examples
#' \dontrun{
#' nba_games <- dfstools::msf_get_feed(
#' "https://api.mysportsfeeds.com/v2.1/pull/nba/2018-playoff/games.json"
#' )}

msf_get_feed <- function(url, verbose = TRUE) {

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
      return(jsonlite::fromJSON(
        httr::content(response, as = "text", encoding = "UTF-8"),
        simplifyVector = TRUE,
        simplifyDataFrame = TRUE,
        simplifyMatrix = TRUE,
        flatten = TRUE)
      )
    }

    # is it a fatal code?
    if (!(status_code %in% retry_allowed)) {
      print(httr::content(response, as = "text", encoding = "UTF-8"))
      stop(status_code)
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
  print(httr::content(response, as = "text", encoding = "UTF-8"))
  stop(status_code)
}

#' @title MySportsFeeds Seasonal Games
#' @name msf_seasonal_games
#' @description Returns a data frame of games from MySportsFeeds version 2.1 API
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr select_if
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr left_join
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
#' @return a tibble of games
#' @details `msf_seasonal_games` adds five columns at the right of the tibble:
#' \itemize{
#'   \item ot - the number of overtime periods / quarters / extra innings,
#'   \item league - the source league of the data,
#'   \item season - the source season of the data,
#'   \item date - the game date (started) in the Eastern USA
#'     timezone ("EST5EDT"), and
#'   \item slug - the game slug (date-away_team-home_team)
#' }
#'
#' The returned tibble will be sorted in order of scheduled start time.
#' @examples
#' \dontrun{
#' nba_games <- dfstools::msf_seasonal_games(
#'   season = "2017-2018-regular", league = "nba"
#' )}

msf_seasonal_games <- function(league, season, verbose = TRUE) {
  response <- dfstools::msf_get_feed(
    dfstools::msf_build_url(
      league, season, "games.json"
    ),
    verbose = verbose
  )
  games <- response[["games"]] %>%
    tibble::as_tibble()
  colnames(games) <- colnames(games) %>%
    snakecase::to_snake_case()

  # how many overtimes were there?
  ot_work <- games %>%
    dplyr::select(schedule_id, .list_column(league)) %>%
    tidyr::unnest(.list_column(league)) %>%
    dplyr::group_by(schedule_id) %>%
    dplyr::summarize(ot = n() - .regulation_periods(league))

  games <- games %>%
    dplyr::left_join(ot_work) %>%
    .good_columns() %>%
    dplyr::mutate(
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
  colnames(games) <- colnames(games) %>% snakecase::to_snake_case()
  return(games %>% dplyr::arrange(schedule_start_time))
}

#' @title MySportsFeeds Seasonal Player Gamelogs
#' @name msf_seasonal_player_gamelogs
#' @description Gets player gamelogs object from from
#' MySportsFeeds version 2.1 API
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
#' @return a `player_gamelogs` object
#' @examples
#' \dontrun{
#' nba_player_gamelogs <- dfstools::msf_seasonal_player_gamelogs(
#'   season = "2018-playoff", league = "nba", team = "GSW"
#' )}

msf_seasonal_player_gamelogs <- function(league, season, team, verbose = TRUE) {
  response <- dfstools::msf_get_feed(
    dfstools::msf_build_url(
      league, season, "player_gamelogs.json", team = team
    ),
    verbose = verbose
  )
  player_gamelogs <- response[["gamelogs"]] %>%
    tibble::as_tibble() %>%
    .good_columns()
  colnames(player_gamelogs) <- colnames(player_gamelogs) %>%
    snakecase::to_snake_case()
  return(player_gamelogs)
}

#' @title MySportsFeeds Seasonal Team Gamelogs
#' @name msf_seasonal_team_gamelogs
#' @description Gets team gamelogs object from from MySportsFeeds
#' version 2.1 API
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
#' @return a `team_gamelogs` object
#' @examples
#' \dontrun{
#' nba_team_gamelogs <- dfstools::msf_seasonal_team_gamelogs(
#'   season = "2018-playoff", league = "nba", team = "GSW"
#' )}

msf_seasonal_team_gamelogs <- function(league, season, team, verbose = TRUE) {
  response <- dfstools::msf_get_feed(
    dfstools::msf_build_url(
      league, season, "team_gamelogs.json", team = team
    ),
    verbose = verbose
  )
  team_gamelogs <- response[["gamelogs"]] %>%
    tibble::as_tibble() %>%
    .good_columns()
  colnames(team_gamelogs) <- colnames(team_gamelogs) %>%
    snakecase::to_snake_case()
  return(team_gamelogs)
}

#' @title MySportsFeeds Seasonal Player Stats Totals
#' @name msf_seasonal_player_stats_totals
#' @description Gets player gamelogs object from from
#' MySportsFeeds version 2.1 API
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
#' @return a `player_stats_totals` object
#' @examples
#' \dontrun{
#' nba_player_totals <- dfstools::msf_seasonal_player_stats_totals(
#'   season = "2018-2019-regular", league = "nba", verbose = TRUE
#' )}

msf_seasonal_player_stats_totals <- function(league, season, verbose = TRUE) {
  response <- dfstools::msf_get_feed(
    dfstools::msf_build_url(
      league, season, "player_stats_totals.json"
    ),
    verbose = verbose
  )
  player_stats_totals <- response[["playerStatsTotals"]] %>%
    tibble::as_tibble() %>%
    .good_columns()
  colnames(player_stats_totals) <- colnames(player_stats_totals) %>%
    snakecase::to_snake_case()
  return(player_stats_totals)
}

utils::globalVariables(c(
  ".",
  "schedule_away_team_abbreviation",
  "schedule_start_time",
  "schedule_id"
))
