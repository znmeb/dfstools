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

#' @title WNBA 2020 Schedule
#' @name wnba_2020_schedule
#' @description fetches the 2020 WNBA schedule from stats.wnba.com and
#' converts it to a format usable by dfstools::mvglmmRank_model
#' @export wnba_2020_schedule
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom jsonlite fromJSON
#' @return a `game.data` tibble
#' @examples
#' \dontrun{
#' wnba_schedule <- dfstools::wnba_2020_schedule()
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

wnba_2020_schedule <- function() {
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

#' @title Basketball Reference WNBA Schedule
#' @name wnba_2020_schedule_bbref
#' @description fetches the 2020 WNBA schedule from Basketball-reference.com and
#' converts it to a format usable by dfstools::mvglmmRank_model
#' @export wnba_2020_schedule_bbref
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom lubridate mdy
#' @importFrom xml2 read_html
#' @return a tibble with three columns
#' \itemize{
#' \item `date` POSIXct the game date,
#' \item `away` character the "away" team
#' \item `home` character the "home" team
#' }
#' @examples
#' \dontrun{
#' wnba_schedule <- dfstools::wnba_2020_schedule_bbref()
#' View(wnba_schedule)
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
