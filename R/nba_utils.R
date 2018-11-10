#' @title Create NBA Database
#' @name create_nba_database
#' @description Creates an SQLite database for all completed NBA seasons with DFS data
#' @importFrom dplyr %>%
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom snakecase to_any_case
#' @export create_nba_database
#' @param sqlite_file a valid file path; it will be overwritten if it exists and created if it doesn't
#' @param verbose print status information
#' @return a DBI connection object pointing to the database

create_nba_database <- function(sqlite_file, verbose = TRUE) {
  unlink(sqlite_file, force = TRUE) # nuke it!
  connection <- connect_database_file(sqlite_file)

  # define the relevant seasons
  seasons <- c(
    "2018-2019-regular",
    "2018 playoff",
    "2017-2018-regular",
    "2017-playoff",
    "2016-2017-regular",
    "2016 playoff",
    "2015-2016-regular"
  )

  # populate the `games`, `teams` and `dates` tables
  for (ixseason in seasons) {
    games <- msf_seasonal_games("nba", ixseason, verbose)$games
    append_table(connection, "games", games)

    teams <- games %>%
      dplyr::select(
        team = schedule_home_team_abbreviation,
        league,
        season
      ) %>% dplyr::distinct()
    append_table(connection, "teams", teams)

    dates <- games %>%
      dplyr::select(
        date,
        league,
        season
      ) %>% dplyr::distinct()
    append_table(connection, "dates", dates)

  }

  # now get tables that must be fetched one team at a time
  teams <- DBI::dbReadTable(connection, "teams")
  for (ixrow in 1:nrow(teams)) {
    ixleague <- teams$league[ixrow]
    ixseason <- teams$season[ixrow]
    ixteam <- teams$team[ixrow]

    # team_gamelogs (team box scores)
    team_gamelogs <- msf_seasonal_team_gamelogs(
      league = ixleague,
      season = ixseason,
      team = ixteam,
      verbose
    )
    status_code <- team_gamelogs[["status_code"]]
    if (status_code == 200) {
      append_table(
        connection, "team_gamelogs", team_gamelogs$team_gamelogs)
    }

    # player_gamelogs (player box scores)
    player_gamelogs <- msf_seasonal_player_gamelogs(
      league = ixleague,
      season = ixseason,
      team = ixteam,
      verbose
    )
    status_code <- player_gamelogs[["status_code"]]
    if (status_code == 200) {
      append_table(
        connection, "player_gamelogs", player_gamelogs$player_gamelogs)
    }

    # DFS
    dfs <- msf_seasonal_dfs(
      league = ixleague,
      season = ixseason,
      team = ixteam,
      verbose
    )
    status_code <- dfs[["status_code"]]
    if (status_code == 200) {
      dfs <- dfs[["dfs"]]
      append_table(connection, "dfs", dfs)
    }
  }
  return(connection)
}

utils::globalVariables(c(
  "league",
  "schedule_home_team_abbreviation",
  "season",
  "team"
))
