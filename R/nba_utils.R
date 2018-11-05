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
