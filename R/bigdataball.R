# internal double digit function
.dd <- function(x) {
  as.integer(x >= 10)
}

# there are two player box score formats from BigDataBall. Some have 23
# columns and some have 24 columns. The 24th column is usage rate as a
# percentage. So we have two different standardized column names
.colnames_23_cols <- c(
  "dataset",
  "game_date",
  "player_name",
  "bdb_position",
  "own_team",
  "opp_team",
  "venue_r_h",
  "minutes",
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
.colnames_24_cols <- c(.colnames_23_cols, "usage_rate")

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
  rx <- df$venue_r_h == "R"
  df$venue_r_h[rx] <- "Road"
  hx <- df$venue_r_h == "H"
  df$venue_r_h[hx] <- "Home"

  # comparable date stamp
  df$game_date <- lubridate::mdy(df$game_date)

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
  df <- pbs %>% dplyr::select(`dataset`:`pts`) %>% dplyr::group_by(
    dataset,
    game_date,
    own_team,
    opp_team,
    venue_r_h
  ) %>%
    dplyr::summarize_if(is.numeric, sum, na.rm = TRUE) %>%
    dplyr::ungroup()
  df$min <- round(df$min, 0)
  return(as.data.frame(df))
}

#' @title read BigDataBall WNBA schedule spreadsheet
#' @name bdb_wnba_schedule
#' @description Reads a BigDataBall schedule spreadsheet to a tibble
#' @export bdb_wnba_schedule
#' @importFrom readxl read_excel
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @param excel_file a BigDataBall Excel schedule file
#' @return a tibble with the schedule

bdb_wnba_schedule <- function(excel_file) {
  return(
    readxl::read_excel(excel_file, col_names = FALSE, skip = 7) %>%
      dplyr::mutate(game_start_time = as.POSIXct(strptime(
        paste(...1, ...2), format = "%Y-%m-%d %I:%M %p", tz = "EST5EDT"))
      ) %>%
      dplyr::select(
        game_start_time,
        road_team = ...5,
        road_rest = ...4,
        home_team = ...8,
        home_rest = ...9
      )
  )
}

#' @title read BigDataBall WNBA DFS spreadsheet
#' @name bdb_wnba_dfs
#' @description Reads a BigDataBall WNBA DFS spreadsheet to a tibble
#' @export bdb_wnba_dfs
#' @importFrom readxl read_excel
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom lubridate mdy
#' @param excel_file a BigDataBall Excel DFS file
#' @return a tibble with the DFS data

bdb_wnba_dfs <- function(excel_file) {
  season_dfs_feed <- readxl::read_excel(excel_file, skip = 2)
  names(season_dfs_feed) <- c(
    "dataset",
    "game_date",
    "player_name",
    "own_team",
    "opp_team",
    "starter_y_n",
    "venue_r_h",
    "minutes",
    "usage_rate",
    "draftkings_position",
    "fanduel_position",
    "draftkings_salary",
    "fanduel_salary",
    "draftkings_points",
    "fanduel_points"
  )
  season_dfs_feed$game_date <- lubridate::mdy(season_dfs_feed$game_date)
  return(season_dfs_feed)
}

#' @title read BigDataBall WNBA team feed spreadsheet
#' @name bdb_wnba_team
#' @description Reads a BigDataBall WNBA DFS spreadsheet to a tibble
#' @export bdb_wnba_team
#' @importFrom readxl read_excel
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @param excel_file a BigDataBall season team Excel file
#' @return a tibble with the team data

bdb_wnba_team <- function(excel_file) {
  raw_data <- readxl::read_excel(excel_file) %>% dplyr::select(
    `DATASET`:`VENUE`, `MIN`:`DEFF`
  )
  #View(raw_data); stop()
  road <- raw_data %>% dplyr::filter(VENUE == "Road") %>%
    dplyr::select(-VENUE)
  names(road) <- c(
    "dataset",
    "game_date",
    "road_team",
    "road_min",
    "road_fg",
    "road_fga",
    "road_3p",
    "road_3pa",
    "road_ft",
    "road_fta",
    "road_or",
    "road_dr",
    "road_tot",
    "road_ast",
    "road_pf",
    "road_stl",
    "road_to",
    "road_blk",
    "road_pts",
    "road_poss",
    "road_pace",
    "road_oeff",
    "road_deff"
  )
  home <- raw_data %>% dplyr::filter(VENUE == "Home") %>%
    dplyr::select(-DATASET, -DATE, -VENUE)
  names(home) <- c(
    "home_team",
    "home_min",
    "home_fg",
    "home_fga",
    "home_3p",
    "home_3pa",
    "home_ft",
    "home_fta",
    "home_or",
    "home_dr",
    "home_tot",
    "home_ast",
    "home_pf",
    "home_stl",
    "home_to",
    "home_blk",
    "home_pts",
    "home_poss",
    "home_pace",
    "home_oeff",
    "home_deff"
  )
  merged <- dplyr::bind_cols(road, home) %>%
    dplyr::mutate(
      num_ot = round((home_min - 200) / 5, 0)
    ) %>% dplyr::select(minutes = road_min, -home_min)
  return( merged)
}

#' @title BigDataBall WNBA mvglmmRank model
#' @name bdb_wnba_rank_model
#' @description creates a WNBA mvglmmRank model
#' @export bdb_wnba_rank_model
#' @importFrom readxl read_excel
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @param season_team_feed a BigDataBall season team feed tibble
#' @return the model returned by dfstools::mvglmmRank_model

bdb_wnba_rank_model <- function(season_team_feed) {
  game_data <- season_team_feed %>%
    dplyr::mutate(
      binary.response = as.integer(home_pts > road_pts),
      neutral.site = 0
    ) %>%
    dplyr::select(
      home = home_team,
      away = road_team,
      home.response = home_pts,
      away.response = road_pts,
      binary.response,
      neutral.site,
      OT = num_ot
    )
    rank_model <- dfstools::mvglmmRank_model(
      game_data, method = "NB", first.order = FALSE, verbose = FALSE)
}

utils::globalVariables(c(
  "DATASET",
  "DATE",
  "VENUE",
  "1Q",
  "OT5",
  "MIN",
  "DEFF",
  "OPENING\r\nODDS",
  "1st HALF\r\nODDS",
  "2nd HALF\r\nODDS",
  "home_closing_odds",
  "road_closing_odds",
  "home_final",
  "home_min",
  "road_final",
  "road_min",
  "...1",
  "...2",
  "...4",
  "...5",
  "...8",
  "...9",
  "dataset",
  "game_start_time",
  "game_date",
  "pts",
  "own_team",
  "opp_team",
  "venue_r_h",
  "binary.response",
  "home_pts",
  "home_team",
  "num_ot",
  "road_pts",
  "road_team"
))
