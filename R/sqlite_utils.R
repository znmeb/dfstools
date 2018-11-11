#' @title SQLite Connect Database File
#' @name sq_connect_dbfile
#' @description creates an SQLite database file if needed and
#' returns its connection
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect
#' @export sq_connect_dbfile
#' @param path full path to file. It will be created if it
#' doesn't exist
#' @return a DBI connection object
#' @examples
#' \dontrun{
#' connection <- dfstools::sq_connect_dbfile(
#' "~/DFS/nba/dfs_database.sqlite"
#' )
#' }

sq_connect_dbfile <- function(path) {
  return(DBI::dbConnect(drv = RSQLite::SQLite(), dbname = path))
}

#' @title SQLite Append Table
#' @name sq_append_table
#' @description creates a table if needed and appends a data
#' frame to it
#' @importFrom DBI dbExistsTable
#' @importFrom DBI dbCreateTable
#' @importFrom DBI dbAppendTable
#' @export sq_append_table
#' @param connection connection to the database
#' @param table the table name. It will be created if it doesn't
#' exist
#' @param dataframe the data frame to append
#' @return a list of two items
#' \itemize{
#' \item table_create logical was the table created?
#' \item table_append integer the number of rows appended to the
#' table
#' }

sq_append_table <- function(connection, table, dataframe) {
  table_create <- FALSE
  if (!DBI::dbExistsTable(connection, table)) {
    table_create <-
      DBI::dbCreateTable(connection, table, dataframe)
  }
  table_append <-
    DBI::dbAppendTable(connection, table, dataframe)
  return(list(
    table_create = table_create, table_append = table_append
  ))
}

#' @title Create Database
#' @name sq_create_database
#' @description Creates an SQLite database for a league and season
#' @importFrom dplyr %>%
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom DBI dbDisconnect
#' @export sq_create_database
#' @param directory valid path to a directory where the database
#' file will be stored. It will be created if it doesn't exist.
#' @param league the league ("nba", "nhl", "nfl" or "mlb")
#' @param season a valid season descriptor for the league
#' @param verbose print status information
#' @return a DBI connection object pointing to the database.
#' `sq_create_database` will disconnect before returning.
#' @details The database will be saved to file
#' `<directory>/<league>_<season>.sqlite`"`. It will be
#' overwritten if it exists and created if it doesn't.

sq_create_database <-
  function(directory, league, season, verbose) {
  dir.create(directory, recursive = TRUE)
  sqlite_file <-
    sprintf("%s/%s_%s.sqlite", directory, league, season)
  if (verbose) {
    print(paste("creating", sqlite_file))
  }
  unlink(sqlite_file, force = TRUE) # nuke it!
  connection <- sq_connect_dbfile(sqlite_file)

  # populate the `games`, `teams` and `dates` tables
  games <- msf_seasonal_games(league, season, verbose)$games
  sq_append_table(connection, "games", games)

  teams <- games %>%
    dplyr::select(
      team = schedule_home_team_abbreviation,
      league,
      season
    ) %>% dplyr::distinct()
  sq_append_table(connection, "teams", teams)

  dates <- games %>%
    dplyr::select(
      date,
      league,
      season
    ) %>% dplyr::distinct()
  sq_append_table(connection, "dates", dates)

  # now get tables that must be fetched one team at a time
  teams <- DBI::dbReadTable(connection, "teams")
  for (ixrow in 1:nrow(teams)) {
    ixteam <- teams$team[ixrow]

    # team_gamelogs (team box scores)
    team_gamelogs <- msf_seasonal_team_gamelogs(
      league, season, ixteam, verbose
    )
    status_code <- team_gamelogs[["status_code"]]
    if (status_code == 200) {
      sq_append_table(
        connection, "team_gamelogs", team_gamelogs$team_gamelogs)
    }

    # player_gamelogs (player box scores)
    player_gamelogs <- msf_seasonal_player_gamelogs(
      league, season, ixteam, verbose
    )
    status_code <- player_gamelogs[["status_code"]]
    if (status_code == 200) {
      sq_append_table(
        connection, "player_gamelogs",
        player_gamelogs$player_gamelogs
      )
    }

    # DFS
    dfs <- msf_seasonal_dfs(
      league, season, ixteam, verbose
    )
    status_code <- dfs[["status_code"]]
    if (status_code == 200) {
      dfs <- dfs[["dfs"]]
      sq_append_table(connection, "dfs", dfs)
    }
  }
  DBI::dbDisconnect(connection)
  return(connection)
}

#' @title SQLite Create Databases
#' @name sq_create_databases
#' @description Creates an SQLite database for each league /
#' season returned by `mfs_seasons()`.
#' @importFrom dplyr %>%
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom DBI dbDisconnect
#' @export sq_create_databases
#' @param directory valid path to a directory where the database
#' files will be stored. It will be created if it doesn't exist.
#' The files will be overwritten if they exist and created if they
#' don't.
#' @param verbose print status information
#' @return a list of the files in `directory` as given by
#' `list.files`.

sq_create_databases <- function(directory, verbose) {
  seasons <- msf_seasons()
  for (ixrow in 1:nrow(seasons)) {
    league <- seasons$league[ixrow]
    season <- seasons$season[ixrow]
    sq_create_database(directory, league, season, verbose)
  }
  return(list.files(directory))
}

utils::globalVariables(c(
  "schedule_home_team_abbreviation"
))
