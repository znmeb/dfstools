Building an NBA DFS Database
================
M. Edward (Ed) Borasky
2018-10-19

## Creating the database

The first order of business is to create an empty database to hold out
NBA data. We use SQLite.

``` r
dir.create("~/DFS/nba", recursive = TRUE)
#> Warning in dir.create("~/DFS/nba", recursive = TRUE): '/home/znmeb/DFS/nba'
#> already exists
unlink("~/DFS/nba/dfs_database.sqlite")
connection <- dfstools::connect_database_file(
  "~/DFS/nba/dfs_database.sqlite")
print(connection)
#> <SQLiteConnection>
#>   Path: /home/znmeb/DFS/nba/dfs_database.sqlite
#>   Extensions: TRUE
DBI::dbListTables(connection)
#> character(0)
```

## Getting the past seasons

The MySportsFeeds API has DFS data for all NBA seasons from the
2015-2016 regular seasons. So we create a list of those seasons and then
fetch them and add them to the database.

``` r
seasons <- c(
  "2015-2016-regular",
  "2016-playoff",
  "2016-2017-regular",
  "2017-playoff",
  "2017-2018-regular",
  "2018-playoff"
)

# you will need to set the enviromnment variable `APIKEY` to your MySportsFeeds API v2.0 API key
apikey <- keyring::key_get("MySportsFeeds")
for (ixseason in seasons) {
  games <- dfstools::msf_seasonal_games(
    "nba", ixseason, apikey) %>% 
    dfstools::select_nba_games_columns()
  status <- dfstools::append_table(
    connection, "games", games
  )
  if (ixseason == "2015-2016-regular") {
    teams <- games %>%     
      dplyr::select(schedule_home_team_abbreviation) %>% 
      dplyr::distinct() %>%
      dplyr::select(team = schedule_home_team_abbreviation) %>% 
      dplyr::arrange(team)
  }
}
print(teams)
#> # A tibble: 30 x 1
#>    team 
#>    <chr>
#>  1 ATL  
#>  2 BOS  
#>  3 BRO  
#>  4 CHA  
#>  5 CHI  
#>  6 CLE  
#>  7 DAL  
#>  8 DEN  
#>  9 DET  
#> 10 GSW  
#> # ... with 20 more rows
all_games <- DBI::dbReadTable(connection, "games")
print(nrow(all_games))
#> [1] 3937
```

## Get the player game logs (box scores) one team at a time

``` r
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
```

## Disconnect from the database

``` r
DBI::dbDisconnect(connection)
```
