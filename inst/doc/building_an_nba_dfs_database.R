## ----setup, include = FALSE----------------------------------------------
source("../.env")
library(magrittr)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
connection <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = "nba_data.sqlite")
print(connection)

if (DBI::dbExistsTable(connection, "nba_games")) {
  DBI::dbRemoveTable(connection, "nba_games")
}
if (DBI::dbExistsTable(connection, "nba_gamelogs")) {
  DBI::dbRemoveTable(connection, "nba_gamelogs")
}
DBI::dbListTables(connection)

## ------------------------------------------------------------------------
seasons <- c(
  "2015-2016-regular",
  "2016-playoff",
  "2016-2017-regular",
  "2017-playoff",
  "2017-2018-regular",
  "2018-playoff"
)

# you will need to set the enviromnment variable `APIKEY` to your MySportsFeeds API v2.0 API key
#apikey <- Sys.getenv("APIKEY")
for (ixseason in seasons) {
  games <-
    dfstools::msf_seasonal_games("nba", ixseason, apikey)$games
  games <- games %>%
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

if (ixseason == "2015-2016-regular") {
    DBI::dbCreateTable(connection, "nba_games", games)
    
    # dbCreateTable creates an empty table, so we append the data
    DBI::dbAppendTable(connection, "nba_games", games)
    teams <- games %>% dplyr::select(schedule_home_team_abbreviation) %>% 
      dplyr::distinct() %>%
      dplyr::select(team = schedule_home_team_abbreviation) %>% 
      dplyr::arrange(team)
  } else {
    DBI::dbAppendTable(connection, "nba_games", games)
  }
}
print(teams)

## ------------------------------------------------------------------------
for (ixteam in teams$team) {
  for (ixseason in seasons) {
    gamelogs <- dfstools::msf_seasonal_player_gamelogs(
      ixseason, "nba", ixteam, apikey)$gamelogs
    if (length(gamelogs) < 1) {next}
    
    colnames(gamelogs) <- colnames(gamelogs) %>% 
      snakecase::to_any_case()
    if (
      ixseason == "2015-2016-regular" &&
      ixteam == "ATL"
    ) {
      columns_to_keep <- grep(
        pattern = "_per_game", 
        x = colnames(gamelogs),
        value = TRUE, 
        invert = TRUE
      )
      gamelogs <- gamelogs %>% dplyr::select_at(
        .vars = dplyr::vars(columns_to_keep)
      )
      DBI::dbCreateTable(connection, "nba_gamelogs", gamelogs)
      DBI::dbAppendTable(connection, "nba_gamelogs", gamelogs)
    } else {
      gamelogs <- gamelogs %>% dplyr::select_at(
        .vars = dplyr::vars(columns_to_keep)
      )
      DBI::dbAppendTable(connection, "nba_gamelogs", gamelogs)
    }
  }
}

DBI::dbDisconnect(connection)


