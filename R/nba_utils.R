#' @title Select NBA Games Columns
#' @name select_nba_games_columns
#' @description Extracts the relevant data from a MySportsFeeds NBA "games" object
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom snakecase to_any_case
#' @export select_nba_games_columns
#' @param games_object a `games` object returmed from msf_seasonal_games for the NBA!
#' @return a `games` data frame with some columns removed
#' @details The NBA `games` object that comes from the NBA has some columns with data issues. Some are all `NA`, and others are list columns that databases can't handle. So we remove them with `dplyr::select`.

select_nba_games_columns <- function(games_object) {
  games <- games_object[["games"]] %>%
    dplyr::select(
      -schedule.endedTime,
      -schedule.originalStartTime,
      -schedule.delayedOrPostponedReason,
      -schedule.attendance:-schedule.weather,
      -score.currentQuarter:-score.currentIntermission,
      -score.quarters
    )
  colnames(games) <- colnames(games) %>%
    snakecase::to_any_case()
  return(games)
}

#' @title Create NBA Database
#' @name create_nba_database
#' @description Creates an SQLite database for all completed NBA seasons with DFS data
#' @importFrom dplyr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr distinct
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @export create_nba_database
#' @param sqlite_file a valid file path; it will be overwritten if it exists and created if it doesn't
#' @param apikey your MuSportsFeeds 2.0 API key
#' @return a DBI connection object pointing to the database

create_nba_database <- function(sqlite_file, apikey) {
  unlink(sqlite_file, force = TRUE) # nuke it!
  connection <- connect_database_file(sqlite_file)

  # define the relevant seasons
  seasons <- c(
    "2015-2016-regular",
    "2016 playoff",
    "2016-2017-regular",
    "2017-playoff",
    "2017-2018-regular",
    "2018 playoff"
  )

  # populate the `games` and `teams` tables
  for (ixseason in seasons) {
    games <- msf_seasonal_games("nba", ixseason, apikey) %>%
      select_nba_games_columns()
    append_table(connection, "games", games)

    teams <- games %>%
      dplyr::select(schedule_home_team_abbreviation) %>%
      dplyr::distinct() %>%
      dplyr::select(team = schedule_home_team_abbreviation) %>%
      dplyr::arrange(team) %>%
      dplyr::mutate(season = ixseason)
    append_table(connection, "teams", teams)
  }

  return(connection)
}

utils::globalVariables(c(
  "apikey",
  "schedule_home_team_abbreviation",
  "team"
))
