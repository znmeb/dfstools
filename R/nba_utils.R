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

#' @title Select NBA Gamelogs Columns
#' @name select_nba_gamelogs_columns
#' @description Extracts the relevant data from a MySportsFeeds NBA "gamelogs" object
#' @importFrom dplyr %>%
#' @importFrom dplyr select_at
#' @importFrom dplyr vars
#' @importFrom snakecase to_any_case
#' @export select_nba_gamelogs_columns
#' @param gamelogs_object a `gamelogs` object returmed from msf_seasonal_gamelogs for the NBA!
#' @return a `gamelogs` data frame with some columns removed
#' @details The NBA `gamelogs` object that comes from the NBA has some columns with data issues. Some are all `NA`, and others are list columns that databases can't handle. So we remove them with `dplyr::select`.

select_nba_gamelogs_columns <- function(gamelogs_object) {
  columns_to_keep <- c(
    "game_id",
    "game_start_time",
    "game_away_team_abbreviation",
    "game_home_team_abbreviation",
    "player_id",
    "player_first_name",
    "player_last_name",
    "player_position",
    "player_jersey_number",
    "team_id",
    "team_abbreviation",
    "stats_field_goals_fg_2_pt_att",
    "stats_field_goals_fg_2_pt_made",
    "stats_field_goals_fg_2_pt_pct",
    "stats_field_goals_fg_3_pt_att",
    "stats_field_goals_fg_3_pt_made",
    "stats_field_goals_fg_3_pt_pct",
    "stats_field_goals_fg_att",
    "stats_field_goals_fg_made",
    "stats_field_goals_fg_pct",
    "stats_free_throws_ft_att",
    "stats_free_throws_ft_made",
    "stats_free_throws_ft_pct",
    "stats_rebounds_off_reb",
    "stats_rebounds_def_reb",
    "stats_rebounds_reb",
    "stats_offense_ast",
    "stats_offense_pts",
    "stats_defense_tov",
    "stats_defense_stl",
    "stats_defense_blk",
    "stats_defense_blk_against",
    "stats_miscellaneous_foul_pers",
    "stats_miscellaneous_plus_minus",
    "stats_miscellaneous_min_seconds"
  )
  gamelogs <- gamelogs_object[["gamelogs"]]
  colnames(gamelogs) <-colnames(gamelogs) %>%
    snakecase::to_any_case()
  gamelogs <- gamelogs %>%
    dplyr::select_at(.vars = dplyr::vars(columns_to_keep))
  return(gamelogs)
}

#' @title Create NBA Database
#' @name create_nba_database
#' @description Creates an SQLite database for all completed NBA seasons with DFS data
#' @importFrom dplyr %>%
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom snakecase to_any_case
#' @export create_nba_database
#' @param sqlite_file a valid file path; it will be overwritten if it exists and created if it doesn't
#' @return a DBI connection object pointing to the database

create_nba_database <- function(sqlite_file) {
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
    games <- msf_seasonal_games("nba", ixseason) %>%
      select_nba_games_columns()
    append_table(connection, "games", games)

    teams <- games %>%
      dplyr::select(
        team = schedule_home_team_abbreviation,
        league,
        season
      ) %>% dplyr::distinct()
    append_table(connection, "teams", teams)
  }

  # now get tables that must be fetched one team at a time
  teams <- DBI::dbReadTable(connection, "teams")
  for (ixrow in 1:nrow(teams)) {
    ixleague <- teams$league[ixrow]
    ixseason <- teams$season[ixrow]
    ixteam <- teams$team[ixrow]

    # gamelogs (player box scores)
    gamelogs <- msf_seasonal_player_gamelogs(
      league = ixleague,
      season = ixseason,
      team = ixteam
    )
    status_code <- gamelogs[["status_code"]]
    if (status_code == 200) {
      gamelogs <- gamelogs %>%
        select_nba_gamelogs_columns()
      append_table(connection, "gamelogs", gamelogs)
    }

    # DFS
    dfs <- msf_seasonal_team_dfs(
      league = ixleague,
      season = ixseason,
      team = ixteam
    )
    status_code <- dfs[["status_code"]]
    if (status_code == 200) {
      dfs <- dfs[["dfs"]]
      colnames(dfs) <- colnames(dfs) %>% snakecase::to_any_case()
      append_table(connection, "dfs", dfs)
    }
  }
  return(connection)
}

utils::globalVariables(c(
  "apikey",
  "league",
  "schedule_home_team_abbreviation",
  "season",
  "team"
))
