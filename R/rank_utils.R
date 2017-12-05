## See <https://github.com/STAT545-UBC/Discussion/issues/451#issuecomment-264598618>
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
  "away",
  "home",
  "home_score_p",
  "away_score_p",
  "home_prob_w",
  "away_prob_w",
  "data_set",
  "games",
  "own_team"
))

#' @title Game predict
#' @name game_predict
#' @description Creates a data frame with game predictions
#' @export game_predict
#' @importFrom dplyr %>%
#' @param model a model from mvglmmRank::mvglmmRank
#' @param schedule a data frame team names in "away" and "home" columns
#' @return the schedule augmented with prediction columns

game_predict <-
  function(model, schedule) {
  aug_schedule <- dplyr::mutate(schedule, method = model$method)

  # are there normal score ratings?
  if (!is.null(model$n.ratings.offense)) {
    aug_schedule <- dplyr::mutate(
      aug_schedule,
      away_score_p =
        model$n.mean["LocationAway"] +
        model$n.ratings.offense[away] -
        model$n.ratings.defense[home],
      home_score_p =
        model$n.mean["LocationHome"] +
        model$n.ratings.offense[home] -
        model$n.ratings.defense[away])
    aug_schedule <- dplyr::mutate(
      aug_schedule,
      total_p =  home_score_p + away_score_p,
      home_mov_p = home_score_p - away_score_p,
      home_ratio_p = home_score_p / away_score_p)
  }

  # are there Poisson score ratings?
  if (!is.null(model$p.ratings.offense)) {
    aug_schedule <- dplyr::mutate(
      aug_schedule,
      away_score_p =
        exp(
          model$p.mean["LocationAway"] +
          model$p.ratings.offense[away] -
          model$p.ratings.defense[home]),
      home_score_p =
        exp(
          model$p.mean["LocationHome"] +
          model$p.ratings.offense[home] -
          model$p.ratings.defense[away]))
    aug_schedule <- dplyr::mutate(
      aug_schedule,
      total_p = home_score_p + away_score_p,
      home_mov_p = home_score_p - away_score_p,
      home_ratio_p = home_score_p / away_score_p)
  }

  # are there binomial win probability ratings?
  if (!is.null(model$b.ratings)) {
    aug_schedule <- dplyr::mutate(
      aug_schedule,
      home_prob_w =
        stats::pnorm(
          model$b.mean +
          model$b.ratings[home] -
          model$b.ratings[away]))
    aug_schedule <- dplyr::mutate(
      aug_schedule,
      away_prob_w = 1 - home_prob_w)
    aug_schedule <- dplyr::mutate(
      aug_schedule,
      entropy =
        -log2(home_prob_w) * home_prob_w - log2(away_prob_w) * away_prob_w)
  }

  return(aug_schedule)
}


## See <https://github.com/STAT545-UBC/Discussion/issues/451#issuecomment-264598618>
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
  "home_mov_p",
  "home_ratio_p",
  "score_p",
  "total_p"
))

#' @title Rank predicted scores
#' @name rank_scores
#' @description rank the teams by predicted scores
#' @export rank_scores
#' @importFrom dplyr %>%
#' @param aug_schedule an augmented schedule from tidy_game_predict
#' @return the teams ranked by ascending scores

rank_scores <- function(aug_schedule) {
  return(dplyr::bind_rows(
    dplyr::select(
      aug_schedule,
      team = away,
      opponent = home,
      score_p = away_score_p,
      total_p,
      home_mov_p,
      home_ratio_p),
    dplyr::select(
      aug_schedule,
      team = home,
      opponent = away,
      score_p = home_score_p,
      total_p,
      home_mov_p,
      home_ratio_p)
    ) %>%
    dplyr::arrange(dplyr::desc(score_p)))
}

#' @title Build model
#' @name build_model
#' @description runs mvglmmRank::mvglmmRank against a game box score and points function
#' @export build_model
#' @importFrom dplyr %>%
#' @param game_data a game.data input file
#' @param method method to be passed to mvglmmRank - default is "PB1"
#' @param first.order flag to be passed to mvglmmRank - default is TRUE
#' @param OT.flag flag to be passes to mvglmmRank - default is TRUE
#' @return an mvglmmRank model object

build_model <- function(
  game_data,
  method = "PB1",
  first.order = TRUE,
  OT.flag = TRUE
) {
  result <- mvglmmRank::mvglmmRank(
    game_data,
    method = method,
    first.order = first.order,
    OT.flag = OT.flag,
    verbose = FALSE)
  return(result)
}

## See <https://github.com/STAT545-UBC/Discussion/issues/451#issuecomment-264598618>
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
  "venue_r_h",
  "opp_team",
  "pts",
  "OT",
  "home.response",
  "away.response"
))

#' @title Collect game data
#' @name game_data
#' @description creates an input game data file for mvglmmRank
#' @export collect_game_data
#' @importFrom dplyr %>%
#' @param gbs game box score data frame
#' @param pmg player minutes per game (240 for NBA, 200 for WNBA)
#' @param end_date the latest date to use as input
#' @return a game data dataframe

collect_game_data <- function(gbs, pmg, end_date) {

  # compute points and OT columns
  pmo <- 25 # player-minutes per overtime
  df <- gbs
  df$OT <- (round(gbs$min, 0) - pmg) / pmo
  df <- dplyr::filter(df, date <= lubridate::ymd(end_date))

  # break out home and away data frames
  home.df <- dplyr::filter(df, venue_r_h == "H")
  away.df <- dplyr::filter(df, venue_r_h == "R")
  home.df <- dplyr::transmute(
    home.df,
    date = date,
    away = opp_team,
    home = own_team,
    home.response = pts)
  away.df <- dplyr::transmute(
    away.df,
    date = date,
    away = own_team,
    home = opp_team,
    away.response = pts,
    OT = OT)
  joined.df <- dplyr::full_join(away.df, home.df) %>%
    dplyr::mutate(
      total = home.response + away.response,
      home_mov = home.response - away.response) %>%
    dplyr::arrange(date)
  joined.df$date <- as.character(joined.df$date)

    return(joined.df)
}
