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
  "own_team",
  "status",
  "started_at",
  "label",
  "home_team_score",
  "away_team_score",
  "at_neutral_site",
  "period",
  "ended_at"
))

#' @title Game predict
#' @name game_predict
#' @description Creates a data frame with game predictions
#' @export game_predict
#' @importFrom dplyr %>%
#' @param model a model from build_model
#' @param season a tibble returned from the Stattleship API via `get_games`
#' @return the upcoming games from the season augmented with prediction columns

game_predict <-
  function(model, season) {
    schedule <- .stattleship_upcoming_games(season)
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
        home_mov_p = home_score_p - away_score_p)
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
        home_mov_p = home_score_p - away_score_p)
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
#' @param season a tibble returned from the Stattleship API via `get_games`
#' @param method method to be passed to mvglmmRank - default is "PB1"
#' @param first.order flag to be passed to mvglmmRank - default is TRUE
#' @param OT.flag flag to be passes to mvglmmRank - default is TRUE
#' @return an mvglmmRank model object

build_model <- function(
  season,
  method = "PB1",
  first.order = TRUE,
  OT.flag = TRUE
) {
  game_data <- .stattleship_game_data(season)
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

## intenral function to extract input for model
.stattleship_game_data <- function(stattleship_games) {

  # compute periods per game for overtime detection
  league = sub("-.*$", "", stattleship_games$slug)[1]
  if (league == "nba" || league == "nfl") ppg <- 4
  if (league == "nhl") ppg <- 3
  game_data <- stattleship_games %>%
    dplyr::filter(status == "closed") %>%
    dplyr::arrange(started_at) %>%
    dplyr::mutate(
      home = sub("^.* vs ", "", label),
      away = sub(" vs .*$", "", label),
      home.response = home_team_score,
      away.response = away_team_score,
      neutral.site = ifelse(is.na(at_neutral_site), 0, 1),
      OT = ifelse(period > ppg, 1, 0)) %>%
    dplyr::select(home:OT, started_at, ended_at)
  return(game_data)
}

# internal function to extract upcoming games
.stattleship_upcoming_games <- function(stattleship_games) {
  game_data <- stattleship_games %>%
    dplyr::filter(status == "upcoming") %>%
    dplyr::arrange(started_at) %>%
    dplyr::mutate(
      home = sub("^.* vs ", "", label),
      away = sub(" vs .*$", "", label)) %>%
    dplyr::select(started_at, away, home)
  return(game_data)
}
