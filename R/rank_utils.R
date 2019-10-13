# Wrappers for mvglmmRank

#' @title Make game.data
#' @name make_game_data
#' @description Builds a `mvglmmRank` "game.data" table from an
#' `msf_seasonal_games`result
#' @export make_game_data
#' @param seasonal_games a tibble returned from `msf_seasonal_games`
#' @return a "game data" table
#' @examples
#' \dontrun{
#' nhl_seasonal_games <- dfstools::msf_seasonal_games("nhl", "2019-2020-regular")
#' nhl_game_data <- dfstools::make_game_data(nhl_seasonal_games)
#' nhl_model <- dfstools::mvglmmRank_model(
#'   nhl_game_data, method = "PB1", first.order = FALSE, verbose = TRUE
#' )
#' nhl_schedule <- dfstools::make_schedule(nhl_seasonal_games)
#' nhl_predictions <- dfstools::game_predict(nhl_schedule, nhl_model)
#' View(nhl_predictions)
#' entropy_forecast <- nhl_predictions %>%
#'   group_by(date) %>%
#'   summarize(games = n(), entropy = sum(entropy))
#' View(entropy_forecast)
#' }

make_game_data <- function(seasonal_games) {
  game.data <- seasonal_games %>% dplyr::filter(
    schedule_played_status == "COMPLETED"
  ) %>% dplyr::mutate(
    home = schedule_home_team_abbreviation,
    away = schedule_away_team_abbreviation,
    home.response = score_home_score_total,
    away.response = score_away_score_total,
    binary.response = as.integer(
      score_home_score_total > score_away_score_total
    ),
    neutral.site = as.integer(schedule_venue_allegiance == "NEUTRAL"),
    OT = ot
  ) %>% dplyr::select(home:OT)
}

#' @title Make schedule
#' @name make_schedule
#' @description Builds a `game_predict` "schedule" table from an
#' `msf_seasonal_games`result
#' @export make_schedule
#' @param seasonal_games a tibble returned from `msf_seasonal_games`
#' @return a "schedule" table
#' @examples
#' \dontrun{
#' nhl_seasonal_games <- dfstools::msf_seasonal_games("nhl", "2019-2020-regular")
#' nhl_game_data <- dfstools::make_game_data(nhl_seasonal_games)
#' nhl_model <- dfstools::mvglmmRank_model(
#'   nhl_game_data, method = "PB1", first.order = FALSE, verbose = TRUE
#' )
#' nhl_schedule <- dfstools::make_schedule(nhl_seasonal_games)
#' nhl_predictions <- dfstools::game_predict(nhl_schedule, nhl_model)
#' View(nhl_predictions)
#' entropy_forecast <- nhl_predictions %>%
#'   group_by(date) %>%
#'   summarize(games = n(), entropy = sum(entropy))
#' View(entropy_forecast)
#' }

make_schedule <- function(seasonal_games) {
  schedule <- seasonal_games %>% dplyr::select(
    date,
    road_team = schedule_away_team_abbreviation,
    home_team = schedule_home_team_abbreviation,
    road_actual_score = score_away_score_total,
    home_actual_score = score_home_score_total
  )
}

#' @title Build an mvglmmRank model
#' @name mvglmmRank_model
#' @description Builds an mvglmmRank model
#' @export mvglmmRank_model
#' @importFrom dplyr %>%
#' @importFrom stats rbinom
#' @importFrom mvglmmRank mvglmmRank
#' @param game_data a `game_data` tibble.
#' @param method the `mvglmmRank` method to use - default is "NB"
#' @param first.order the `mvglmmRank` first-order correction flag -
#' default is `FALSE`
#' @param verbose print a lot of stuff whilst iterating - default is `FALSE`
#' @return an mvglmmRank model object
#' @examples
#' \dontrun{
#' nhl_seasonal_games <- dfstools::msf_seasonal_games("nhl", "2019-2020-regular")
#' nhl_game_data <- dfstools::make_game_data(nhl_seasonal_games)
#' nhl_model <- dfstools::mvglmmRank_model(
#'   nhl_game_data, method = "PB1", first.order = FALSE, verbose = TRUE
#' )
#' nhl_schedule <- dfstools::make_schedule(nhl_seasonal_games)
#' nhl_predictions <- dfstools::game_predict(nhl_schedule, nhl_model)
#' View(nhl_predictions)
#' entropy_forecast <- nhl_predictions %>%
#'   group_by(date) %>%
#'   summarize(games = n(), entropy = sum(entropy))
#' View(entropy_forecast)
#' }

mvglmmRank_model <- function(
  game_data, method = "NB", first.order = FALSE, verbose = FALSE) {
  print(start_time <- Sys.time())
  nb_model <- mvglmmRank::mvglmmRank(
    game.data = game_data,
    method = method,
    first.order = first.order,
    home.field = TRUE,
    OT.flag = TRUE,
    Hessian = FALSE,
    max.iter.EM = 2000,
    tol1 = 1e-03,
    tol2 = 1e-03,
    verbose = verbose)
  print(end_time <- Sys.time())
  print(end_time - start_time)
  return(nb_model)
}

#' @title Game predict
#' @name game_predict
#' @description Creates a data frame with game predictions
#' @export game_predict
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @param schedule a data frame team names in "road_team" and "home_team" columns
#' @param model a model from mvglmmRank::mvglmmRank
#' @return the schedule augmented with prediction columns
#' @examples
#' \dontrun{
#' nhl_seasonal_games <- dfstools::msf_seasonal_games("nhl", "2019-2020-regular")
#' nhl_game_data <- dfstools::make_game_data(nhl_seasonal_games)
#' nhl_model <- dfstools::mvglmmRank_model(
#'   nhl_game_data, method = "PB1", first.order = FALSE, verbose = TRUE
#' )
#' nhl_schedule <- dfstools::make_schedule(nhl_seasonal_games)
#' nhl_predictions <- dfstools::game_predict(nhl_schedule, nhl_model)
#' View(nhl_predictions)
#' entropy_forecast <- nhl_predictions %>%
#'   group_by(date) %>%
#'   summarize(games = n(), entropy = sum(entropy))
#' View(entropy_forecast)
#' }

game_predict <-
  function(schedule, model) {
    aug_schedule <- schedule %>%
      dplyr::mutate(method = model$method)

    # are there normal score ratings?
    if (!is.null(model$n.ratings.offense)) {
      aug_schedule <- aug_schedule %>% dplyr::mutate(
        road_team_score_p =
          round(model$n.mean["LocationAway"] +
          model$n.ratings.offense[road_team] -
          model$n.ratings.defense[home_team], 1),
        home_team_score_p =
          round(model$n.mean["LocationHome"] +
          model$n.ratings.offense[home_team] -
          model$n.ratings.defense[road_team], 1),
        total_p =  round(home_team_score_p + road_team_score_p, 0),
        home_team_mov_p = round(home_team_score_p - road_team_score_p, 1))
    }

    # are there Poisson score ratings?
    if (!is.null(model$p.ratings.offense)) {
      aug_schedule <- aug_schedule %>% dplyr::mutate(
        road_team_score_p =
          round(exp(
            model$p.mean["LocationAway"] +
              model$p.ratings.offense[road_team] -
              model$p.ratings.defense[home_team]), 1),
        home_team_score_p =
          round(exp(
            model$p.mean["LocationHome"] +
              model$p.ratings.offense[home_team] -
              model$p.ratings.defense[road_team]), 1),
        total_p = round(home_team_score_p + road_team_score_p, 0),
        home_team_mov_p = round(home_team_score_p - road_team_score_p, 1))
    }

    # are there binomial win probability ratings?
    if (!is.null(model$b.ratings)) {
      aug_schedule <- aug_schedule %>% dplyr::mutate(
        home_team_prob_w =
          round(stats::pnorm(
            model$b.mean +
              model$b.ratings[home_team] -
              model$b.ratings[road_team]), 3),
        road_team_prob_w = round(1 - home_team_prob_w, 3),
        entropy = round(-log2(home_team_prob_w) * home_team_prob_w -
          log2(road_team_prob_w) * road_team_prob_w, 3))
    }

    return(aug_schedule)
  }

## global name declarations
## See <https://github.com/STAT545-UBC/Discussion/issues/451#issuecomment-264598618>
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
  "OT",
  "ot",
  "schedule_played_status",
  "schedule_venue_allegiance",
  "score_away_score_total",
  "score_home_score_total",
  "road_team_prob_w",
  "road_team_score_p",
  "home_team_prob_w",
  "home_team_score_p",
  "LScore",
  "LTeamID",
  "NumOT",
  "WLoc",
  "WLocX",
  "WScore",
  "WTeamID",
  "away",
  "away.response",
  "home",
  "home_team",
  "home.response",
  "Season",
  "neutral.site",
  "road_team",
  "TeamID",
  "TeamName",
  "desc",
  "value",
  "Rating"))
