#' @title WNBA season player totals from wnba.com
#' @name wnba_season_totals_wnba
#' @description fetches the WNBA player totals for a given season from
#' wnba.com and prepares the raw data for archetypal
#' analysis.
#' @export wnba_season_totals_wnba
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom tibble remove_rownames
#' @importFrom janitor clean_names
#' @importFrom rvest html_table
#' @importFrom xml2 read_html
#' @param season season to fetch (1997 - 2020)
#' @return a list of two items
#' \itemize{
#' \item player_totals a tibble of player season total box score statistics,
#' arranged by descending points scored
#' \item player_labels a tibble of labeling information for players
#' }
#' @examples
#' \dontrun{
#' wnba_totals <- dfstools::wnba_season_totals_wnba(2020)
#' player_totals <- wnba_totals$player_totals
#' player_labels <- wnba_totals$player_labels
#' the_archetypes <- dfstools::wnba_archetypes(player_totals)
#' player_alphas <- the_archetypes[["player_alphas"]]
#' archetype_parameters <- the_archetypes[["archetype_parameters"]]
#' View(player_alphas)
#' View(archetype_parameters)
#' }

wnba_season_totals_wnba <- function(season) {
  if (season < 1997 | season > 2020) {
    stop(paste("season", season, "is invalid!"))
  }
  url <- paste0(
    "https://stats.wnba.com/stats/leagueLeaders?LeagueID=10&PerMode=Totals&Scope=S&Season=",
    season,
    "&SeasonType=Regular+Season&StatCategory=PTS"
  )
  json <- jsonlite::fromJSON(url, flatten = TRUE)
  raw_data <- as.data.frame(json[["resultSet"]][["rowSet"]])
  names(raw_data) <- janitor::make_clean_names(json[["resultSet"]][["headers"]])
  for (ixcol in 5:ncol(raw_data)) {
    numbers <- as.numeric(raw_data[, ixcol])
    numbers[is.na(numbers)] <- 0
    raw_data[, ixcol] <- numbers
  }
  raw_data <- raw_data %>% dplyr::mutate(
    player_name = paste(player, team),
    fg2a = fga - fg3a,
    fg2m = fgm - fg3m
  )

  label_columns <- c(
    "player_name",
    "team"
  )
  player_labels <- raw_data %>%
    dplyr::select(label_columns) %>%
    dplyr::arrange(player_name) %>%
    unique()

  stats_columns <- c(
    "player_name",
    "min",
    "gp",
    "fg2a",
    "fg2m",
    "fg3a",
    "fg3m",
    "fga",
    "fgm",
    "fta",
    "ftm",
    "oreb",
    "dreb",
    "reb",
    "ast",
    "pts",
    "tov",
    "stl",
    "blk",
    "pf"
  )
  player_totals <- raw_data %>%
    dplyr::select(stats_columns) %>%
    dplyr::arrange(desc(pts)) %>%
    unique()

  return(list(
    player_totals = player_totals,
    player_labels = player_labels)
  )

}

#' @title WNBA season player totals from Basketball Reference
#' @name wnba_season_totals_bbref
#' @description fetches the WNBA player totals for a given season from
#' basketball-reference.com and prepares the raw data for archetypal
#' analysis.
#' @export wnba_season_totals_bbref
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom tibble remove_rownames
#' @importFrom janitor clean_names
#' @importFrom rvest html_table
#' @importFrom xml2 read_html
#' @param season season to fetch (1997 - 2020)
#' @return a list of two items
#' \itemize{
#' \item player_totals a tibble of player season total box score statistics,
#' arranged by descending points scored
#' \item player_labels a tibble of labeling information for players
#' }
#' @examples
#' \dontrun{
#' wnba_totals <- dfstools::wnba_season_totals_bbref(2020)
#' player_totals <- wnba_totals$player_totals
#' player_labels <- wnba_totals$player_labels
#' the_archetypes <- dfstools::wnba_archetypes(player_totals)
#' player_alphas <- the_archetypes[["player_alphas"]]
#' archetype_parameters <- the_archetypes[["archetype_parameters"]]
#' View(player_alphas)
#' View(archetype_parameters)
#' }

wnba_season_totals_bbref <- function(season) {
  if (season < 1997 | season > 2020) {
    stop(paste("season", season, "is invalid!"))
  }
  url <- paste0(
    "https://www.basketball-reference.com/wnba/years/",
    season,
    "_totals.html"
  )
  raw_data <- xml2::read_html(url) %>% rvest::html_table()
  raw_data <- raw_data[[1]]
  for (ixcol in 4:ncol(raw_data)) {
    numbers <- as.numeric(raw_data[, ixcol])
    numbers[is.na(numbers)] <- 0
    raw_data[, ixcol] <- numbers
  }
 raw_data <- raw_data %>%
    janitor::clean_names() %>%
    dplyr::filter(player != "Player") %>%
    dplyr::mutate(
      player_name = paste(player, tm),
      drb = trb - orb
    )

  label_columns <- c(
    "player_name",
    "tm",
    "pos"
  )
  player_labels <- raw_data %>%
    dplyr::select(label_columns) %>%
    dplyr::arrange(player_name) %>%
    unique()

  stats_columns <- c(
    "player_name",
    "mp",
    "g",
    "gs",
    "x2pa",
    "x2p",
    "x3pa",
    "x3p",
    "fga",
    "fg",
    "fta",
    "ft",
    "orb",
    "drb",
    "trb",
    "ast",
    "pts",
    "tov",
    "stl",
    "blk",
    "pf"
  )
  player_totals <- raw_data %>%
    dplyr::select(stats_columns) %>%
    dplyr::arrange(desc(pts)) %>%
    unique()

  return(list(
    player_totals = player_totals,
    player_labels = player_labels)
  )

}

#' @title WNBA Archetypal Analysis
#' @name wnba_archetypes
#' @description perform an "archetypal athletes" analysis
#' @importFrom archetypes archetypes
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom tibble as_tibble
#' @export wnba_archetypes
#' @param player_totals a tibble returned by `nba_player_season_totals`
#' @param num_archetypes number of archetypes to use (default 3)
#' @return a list of
#' \itemize{
#' \item archetype_parameters the parameters that define each archetype
#' \item player_alphas the players tagged with their loadings on each archetype
#' \item archetype_model the model object}
#' @examples
#' \dontrun{
#' wnba_totals <- dfstools::wnba_season_totals_bbref(2020)
#' player_totals <- wnba_totals$player_totals
#' player_labels <- wnba_totals$player_labels
#' the_archetypes <- dfstools::wnba_archetypes(player_totals)
#' player_alphas <- the_archetypes[["player_alphas"]]
#' archetype_parameters <- the_archetypes[["archetype_parameters"]]
#' View(player_alphas)
#' View(archetype_parameters)
#' }

wnba_archetypes <- function(player_totals, num_archetypes = 3) {
  return(compute_archetypes(player_totals, num_archetypes))
}

#' @title WNBA Archetype Search
#' @name wnba_archetype_search
#' @description stepwise search of archetype counts
#' @importFrom archetypes stepArchetypes
#' @importFrom archetypes bestModel
#' @importFrom archetypes robustArchetypes
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom tibble as_tibble
#' @export wnba_archetype_search
#' @param player_totals a tibble returned by `nba_player_season_totals`
#' @param num_steps number of steps to use (default 1:10)
#' @param nrep number of repetitions at each step (default 64)
#' @param verbose should the search be verbose? (default TRUE)
#' @return a list of
#' \itemize{
#' \item archetype_parameters the parameters that define each archetype
#' \item player_alphas the players tagged with their loadings on each archetype
#' \item archetype_model the model object - the `bestModel` with `num_steps`
#' archetypes
#' \item all of the models}
#' @examples
#' \dontrun{
#' wnba_totals <- dfstools::wnba_season_totals_bbref(2020)
#' player_totals <- wnba_totals$player_totals
#' player_labels <- wnba_totals$player_labels
#' the_archetypes <- dfstools::wnba_archetype_search(player_totals)
#' screeplot(the_archetypes$archetype_models)
#' }

wnba_archetype_search <-
  function(player_totals, num_steps = 1:10, nrep = 64, verbose = TRUE) {
    return(archetype_search(player_totals, num_steps, nrep, verbose))
  }

#' @title Make WNBA game.data
#' @name wnba_make_game_data
#' @description Builds a `mvglmmRank` "game.data" table from a
#' `wnba_2020_schedule` result
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @export wnba_make_game_data
#' @param wnba_schedule a tibble returned from `wnba_2020_schedule`
#' @return a `game.data` table
#' @examples
#' \dontrun{
#' wnba_schedule <- dfstools::wnba_2020_schedule()
#' wnba_game_data <- dfstools::wnba_make_game_data(wnba_schedule)
#' View(wnba_game_data)
#' }

wnba_make_game_data <- function(wnba_schedule) {
  wnba_schedule %>% dplyr::filter(!is.na(away.response))
}

#' @title WNBA 2020 Schedule from WNBA site
#' @name wnba_2020_schedule_wnba
#' @description fetches the 2020 WNBA schedule from stats.wnba.com and
#' converts it to a format usable by dfstools::mvglmmRank_model
#' @export wnba_2020_schedule_wnba
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom jsonlite fromJSON
#' @return a tibble that can be truncated to serve as a `game.data` input for
#' `dfstools::mvglmmRank_model` and as a `schedule` input for
#' `dfstools::game_predict`.
#' @examples
#' \dontrun{
#' wnba_schedule <- dfstools::wnba_2020_schedule_wnba()
#' wnba_game_data <- dfstools::wnba_make_game_data(wnba_schedule)
#' wnba_model <- dfstools::mvglmmRank_model(wnba_game_data, verbose = FALSE)
#' teams <- as.character(names(wnba_model[["n.ratings.offense"]]))
#' ratings <- tibble::as_tibble(list(
#'   team = teams,
#'   offense = wnba_model[["n.ratings.offense"]],
#'   defense = wnba_model[["n.ratings.defense"]]
#' ))
#' forecast <- dfstools::game_predict(
#'   schedule = wnba_schedule %>% dplyr::rename(
#'     road_team = away,
#'     home_team = home
#'   ),
#'   model = wnba_model
#' )
#' entropy <- forecast %>%
#'   dplyr::group_by(date) %>%
#'   dplyr::summarise(total_entropy = sum(entropy))
#' }

wnba_2020_schedule_wnba <- function() {
  raw_data <- fromJSON(
    "https://data.wnba.com/data/5s/v2015/json/mobile_teams/wnba/2020/league/10_full_schedule.json",
    flatten = TRUE
  )
  schedule <- dplyr::bind_rows(
    raw_data[["lscd"]][["mscd.g"]][[1]],
    raw_data[["lscd"]][["mscd.g"]][[2]],
    raw_data[["lscd"]][["mscd.g"]][[3]]
  ) %>% dplyr::mutate(
    date = gdte,
    away = paste(v.tc, v.tn, sep = " "),
    away.response = as.integer(v.s),
    home = paste(h.tc, h.tn, sep = " "),
    home.response = as.integer(h.s),
    neutral.site = 0,
    binary.response = as.integer(home.response > away.response)
  ) %>% dplyr::select(date:binary.response)
}

#' @title WNBA BigDataBall Team Box Score
#' @name wnba_bdb_team_box_score
#' @description fetches a BigDataBall WNBA team box score
#' @export wnba_bdb_team_box_score
#' @importFrom dplyr %>%
#' @importFrom janitor clean_names
#' @importFrom readxl read_excel
#' @param bdb_excel_file a BigDataBall WNBA team box score file
#' @return a tibble with the team box score
#' @examples
#' \dontrun{
#' wnba_team_box_score <- dfstools::wnba_bdb_team_box_score(
#'   "~/Dropbox/20-wnba-team/08-09-2020-wnba-season-team-feed.xlsx"
#' )
#' View(wnba_team_box_score)
#' }

wnba_bdb_team_box_score <- function(bdb_excel_file) {
  readxl::read_excel(bdb_excel_file, col_types = c(
    "text", "text", "text", "text", "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric", "skip", "skip", "skip", "skip",
    "skip"
  )) %>%
    janitor::clean_names()
}

#' @title WNBA BigDataBall DFS
#' @name wnba_bdb_dfs
#' @description fetches a BigDataBall WNBA player box score
#' @export wnba_bdb_dfs
#' @importFrom readxl read_excel
#' @param bdb_excel_file a BigDataBall WNBA DFS file
#' @return a tibble with the DFS data
#' @examples
#' \dontrun{
#' wnba_dfs <- dfstools::wnba_bdb_dfs(
#'   "~/Dropbox/20-wnba-dfs/08-07-2020-wnba-season-dfs-feed.xlsx"
#' )
#' View(wnba_dfs)
#' }

wnba_bdb_dfs <- function(bdb_excel_file) {
  df <- readxl::read_excel(
    bdb_excel_file,
    col_names = FALSE,
    skip = 3
  )
  names(df) <- c(
    "dataset",
    "date",
    "player_full_name",
    "own_team",
    "opp_team",
    "starter",
    "venue_road_home",
    "min",
    "usage_rate_percent",
    "draftkings_position",
    "fanduel_position",
    "draftkings_salary",
    "fanduel_salary",
    "draftkings_points",
    "fanduel_points"
  )
  return(df)
}

#' @title WNBA BigDataBall Player Box Score
#' @name wnba_bdb_player_box_score
#' @description fetches a BigDataBall WNBA player box score
#' @export wnba_bdb_player_box_score
#' @importFrom dplyr %>%
#' @importFrom janitor clean_names
#' @importFrom readxl read_excel
#' @param bdb_excel_file a BigDataBall WNBA player box score file
#' @return a tibble with the player box score
#' @examples
#' \dontrun{
#' wnba_player_box_score <- dfstools::wnba_bdb_player_box_score(
#'   "~/Dropbox/20-wnba-player/08-07-2020-wnba-season-player-feed.xlsx"
#' )
#' View(wnba_player_box_score)
#' }

wnba_bdb_player_box_score <- function(bdb_excel_file) {
  readxl::read_excel(bdb_excel_file) %>%
    janitor::clean_names()
}

#' @title WNBA 2020 Schedule from Basketball Reference
#' @name wnba_2020_schedule_bbref
#' @description fetches the 2020 WNBA schedule from Basketball-reference.com and
#' converts it to a format usable by dfstools::mvglmmRank_model
#' @export wnba_2020_schedule_bbref
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom lubridate mdy
#' @importFrom xml2 read_html
#' @return a tibble that can be truncated to serve as a `game.data` input for
#' `dfstools::mvglmmRank_model` and as a `schedule` input for
#' `dfstools::game_predict`.
#' @examples
#' \dontrun{
#' wnba_schedule <- dfstools::wnba_2020_schedule_bbref()
#' wnba_game_data <- dfstools::wnba_make_game_data(wnba_schedule)
#' wnba_model <- dfstools::mvglmmRank_model(wnba_game_data, verbose = FALSE)
#' teams <- as.character(names(wnba_model[["n.ratings.offense"]]))
#' ratings <- tibble::as_tibble(list(
#'   team = teams,
#'   offense = wnba_model[["n.ratings.offense"]],
#'   defense = wnba_model[["n.ratings.defense"]]
#' ))
#' forecast <- dfstools::game_predict(
#'   schedule = wnba_schedule %>% dplyr::rename(
#'     road_team = away,
#'     home_team = home
#'   ),
#'   model = wnba_model
#' )
#' entropy <- forecast %>%
#'   dplyr::group_by(date) %>%
#'   dplyr::summarise(total_entropy = sum(entropy))
#' }

wnba_2020_schedule_bbref <- function() {
  df <- xml2::read_html(
    "https://www.basketball-reference.com/wnba/years/2020-schedule.html"
  ) %>% rvest::html_table()
  df <- df[[1]]
  names(df) <- c(
    "raw_date",
    "away",
    "away.response",
    "home",
    "home.response",
    "discard"
  )
  df %>% dplyr::mutate(
    date = lubridate::mdy(raw_date),
    neutral.site = 0,
    binary.response = as.integer(home.response > away.response)
  ) %>%
    dplyr::select(date, away:home.response, neutral.site:binary.response)
}

#' @title Rest days
#' @name wnba_rest_days
#' @description Compute rest days from the schedule
#' @export wnba_rest_days
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_rows
#' @importFrom dplyr lag
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @param schedule a tibble returned by `wnba_2020_schedule_bbref`
#' @return a tibble with three columns
#' \itemize{
#' \item `team` character team name,
#' \item `date` POSIXct the game date,
#' \item `rest_days` numeric number of rest days
#' }
#' @examples
#' \dontrun{
#' wnba_schedule <- dfstools::wnba_2020_schedule_bbref()
#' View(wnba_schedule)
#' rest_days <- dfstools::wnba_rest_days(wnba_schedule)
#' View(rest_days)
#' }

wnba_rest_days <- function(schedule) {
  aways <- schedule %>% dplyr::select(team = away, date)
  homes <- schedule %>% dplyr::select(team = home, date)
  aways %>% dplyr::bind_rows(homes) %>%
    dplyr::arrange(team, date) %>%
    dplyr::group_by(team) %>%
    dplyr::mutate(rest_days = as.numeric(date - dplyr::lag(date)) - 1) %>%
    dplyr::ungroup()
}

## global name declarations - See
## https://github.com/STAT545-UBC/Discussion/issues/451#issuecomment-264598618
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
  "fg3a",
  "fg3m",
  "fga",
  "fgm",
  "tm",
  "g",
  "orb",
  "player",
  "pos",
  "pts",
  "trb",
  "gdte",
  "h.s",
  "h.tc",
  "h.tn",
  "v.s",
  "v.tc",
  "v.tn",
  "binary.response",
  "raw_date",
  "team"
))
