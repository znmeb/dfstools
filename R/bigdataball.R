# internal double digit function
.dd <- function(x) {
  as.integer(x >= 10)
}

# there are two player box score formats from BigDataBall. Some have 23
# columns and some have 24 columns. The 24th column is usage rate as a
# percentage. So we have two different standardized column names
.colnames_23_cols <- c(
  "data_set",
  "date",
  "player_full_name",
  "position",
  "own_team",
  "opp_team",
  "venue_r_h",
  "min",
  "fg",
  "fga",
  "x3p",
  "x3pa",
  "ft",
  "fta",
  "or",
  "dr",
  "tot",
  "a",
  "pf",
  "st",
  "to",
  "bl",
  "pts"
)
.colnames_24_cols <- c(.colnames_23_cols, "usage_rate_percent")

#  Points calculators
#' @title Real-world points
#' @name real_points
#' @description Computes points from a box score
#' @export real_points
#' @param boxscore a player or game box score data frame
#' @return a vector of points
#' @examples
#' \dontrun{
#' playerbox <- player_box_score(
#'   "~/Dropbox/16-17-player-data/season-player-feed-02-03-2017.xlsx")
#' points <- real_points(playerbox)
#' }

real_points <- function(boxscore) {
  return(boxscore$pts)
}

#' @title Game Score (GmSc)
#' @name game_score
#' @description Computesthe "game score" metric from a box score
#' @export game_score
#' @param boxscore a player or game box score data frame
#' @return a vector of game scores
#' @examples
#' \dontrun{
#' playerbox <- player_box_score(
#'   "~/Dropbox/16-17-player-data/season-player-feed-02-03-2017.xlsx")
#' gmsc <- game_score(playerbox)
#' }

game_score <- function(boxscore) {
  result <-
    1.0 * boxscore$pts +
    0.4 * boxscore$fg +
    -0.7 * boxscore$fga +
    -0.4 * (boxscore$fta - boxscore$ft) +
    0.7 * boxscore$or +
    0.3 * boxscore$dr +
    0.7 * boxscore$a +
    1.0 * boxscore$st +
    0.7 * boxscore$bl +
    -0.4 * boxscore$pf +
    -1.0 * boxscore$to
  return(round(result, 1))
}

#' @title DraftKings fantasy points
#' @name draftkings_points
#' @description Computes DraftKings fantasy points from a box score
#' @export draftkings_points
#' @param boxscore a player or game box score data frame
#' @return a vector of DraftKings fantasy points
#' @examples
#' \dontrun{
#' playerbox <- player_box_score(
#'   "~/Dropbox/16-17-player-data/season-player-feed-02-03-2017.xlsx")
#' dk_points <- draftkings_points(playerbox)
#' }

draftkings_points <- function(boxscore) {
  1.0 * boxscore$pts +
    0.5 * boxscore$x3p +
    1.25 * boxscore$tot +
    1.5 * boxscore$a +
    2.0 * boxscore$st +
    2.0 * boxscore$bl +
    -0.5 * boxscore$to +
    1.5 * boxscore$ddbl +
    3.0 * boxscore$tdbl
}

#' @title FanDuel fantasy points
#' @name fanduel_points
#' @description Computes FanDuel fantasy points from a box score
#' @export fanduel_points
#' @param boxscore a player or game box score data frame
#' @return a vector of FanDuel fantasy points
#' @examples
#' \dontrun{
#' playerbox <- player_box_score(
#'   "~/Dropbox/16-17-player-data/season-player-feed-02-03-2017.xlsx")
#' fd_points <- fanduel_points(playerbox)
#' }

fanduel_points <- function(boxscore) {
  1.0 * boxscore$pts +
  1.2 * boxscore$tot +
  1.5 * boxscore$a +
  2.0 * boxscore$st +
  2.0 * boxscore$bl +
  -1.0 * boxscore$to
}

#' @title Yahoo fantasy points
#' @name yahoo_points
#' @description Computes Yahoo fantasy points from a box score
#' @export yahoo_points
#' @param boxscore a player box score data frame
#' @return a vector of Yahoo fantasy points
#' @examples
#' \dontrun{
#' playerbox <- player_box_score(
#'   "~/Dropbox/16-17-player-data/season-player-feed-02-03-2017.xlsx")
#' yh_points <- yahoo_points(playerbox)
#' }

yahoo_points <- function(boxscore) {
  1.0 * boxscore$pts +
  0.5 * boxscore$x3p +
  1.2 * boxscore$tot +
  1.5 * boxscore$a +
  2.0 * boxscore$st +
  2.0 * boxscore$bl +
  -1.0 * boxscore$to
}

#' @title Make player box score data frame
#' @name player_box_score
#' @description Creates a player box score data frame from an BigDataBall spreadsheet
#' @export player_box_score
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom readxl read_excel
#' @importFrom lubridate mdy
#' @param spreadsheet a file with an BigDataBall player box score dataset
#' @param sheet_number the sheet number within the spreadsheet (default 1)
#' @return a data frame of the input spreadsheet, augmented with columns for
#' double-doubles, triple doubles, "game scores" and fantasy points. Note that
#' since turnovers are bad, the turnover column is the negative of the numbe
#' of turnovers!
#' @examples
#' \dontrun{
#' playerbox <- player_box_score(
#'   "~/Dropbox/16-17-player-data/season-player-feed-02-03-2017.xlsx")
#' }

player_box_score <- function(spreadsheet, sheet_number = 1) {
  df <- readxl::read_excel(spreadsheet, sheet = sheet_number) %>%
    dplyr::filter(!is.na("DATE"))
  if (ncol(df) == 24) {
    colnames(df) <- .colnames_24_cols
  } else {
    colnames(df) <- .colnames_23_cols
  }

  # enforce venue code consistency
  rx <- df$venue_r_h == "Road"
  df$venue_r_h[rx] <- "R"
  hx <- df$venue_r_h == "Home"
  df$venue_r_h[hx] <- "H"

  # comparable date stamp
  df$date <- lubridate::mdy(df$date)

  # compute double-double, triple-double and fantasy points
  count <- .dd(df$tot) + .dd(df$a) + .dd(df$st) + .dd(df$bl) + .dd(df$pts)
  df$ddbl <- as.integer(count >= 2)
  df$tdbl <- as.integer(count >= 3)
  df$gmsc <- game_score(df)
  df$fdfp <- fanduel_points(df)
  df$dkfp <- draftkings_points(df)
  df$yfp <- yahoo_points(df)
  df$to <- -df$to # negate turnovers so high numbers are good ;-)
  df$games <- 1

  # result
  return(df)
}

#' @title Make game box score data frame from player box score
#' @name game_box_score
#' @description Creates a game box score data frame from a player box score data frame
#' @export game_box_score
#' @importFrom dplyr %>%
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @param pbs a player box score data frame
#' @return a data frame of game box scores
#' @examples
#' \dontrun{
#' pbs2015 <- player_box_score(
#'   "~/Dropbox/15-16-player-data/season-player-feed-01-28-2016.xlsx")
#' gbs2015 <- game_box_score(pbs2015)
#' }

game_box_score <- function(pbs) {
  df <- pbs %>% dplyr::select(`data_set`:`pts`) %>% dplyr::group_by(
    data_set,
    date,
    own_team,
    opp_team,
    venue_r_h
  ) %>%
    dplyr::summarize_if(is.numeric, sum, na.rm = TRUE) %>%
    dplyr::ungroup()
  df$min <- round(df$min, 0)
  return(as.data.frame(df))
}

utils::globalVariables(c(
  "data_set",
  "pts",
  "own_team",
  "opp_team",
  "venue_r_h"
))
