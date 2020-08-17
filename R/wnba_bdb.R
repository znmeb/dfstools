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

## global name declarations - See
## https://github.com/STAT545-UBC/Discussion/issues/451#issuecomment-264598618
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
))
