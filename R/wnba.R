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
#' @description fetches the 2020 WNBA schedule from Basketball-reference.com
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
  df[[1]] %>% dplyr::select(
    raw_date = "Date",
    away = "Visitor/Neutral",
    home = "Home/Neutral") %>%
    dplyr::mutate(
      date = lubridate::mdy(raw_date),
    )
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
  "raw_date",
  "team"
))
