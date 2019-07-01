# Wrappers for mvglmmRank

#' @title Build an mvglmmRank model
#' @name mvglmmRank_model
#' @description Builds an mvglmmRank model
#' @export mvglmmRank_model
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom stats rbinom
#' @importFrom mvglmmRank mvglmmRank
#' @param game_data a `game_data` tibble.
#' @param method the `mvglmmRank` method to use - default is "NB.mov"
#' @param first.order the `mvglmmRank` first-order correction flag -
#' default is `TRUE`
#' @param verbose print a lot of stuff whilst iterating - default is `FALSE`
#' @return an mvglmmRank model object

mvglmmRank_model <- function(
  game_data, method = "NB.mov", first.order = TRUE, verbose = FALSE) {
  print(start_time <- Sys.time())
  nb_model <- mvglmmRank::mvglmmRank(
    game.data = game_data,
    method = method,
    first.order = first.order,
    home.field = TRUE,
    OT.flag = TRUE,
    Hessian = FALSE,
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
#' @param schedule a data frame team names in "away" and "home" columns
#' @param model a model from mvglmmRank::mvglmmRank
#' @return the schedule augmented with prediction columns

game_predict <-
  function(schedule, model) {
    aug_schedule <- schedule %>%
      dplyr::mutate(method = model$method)

    # are there normal score ratings?
    if (!is.null(model$n.ratings.offense)) {
      aug_schedule <- aug_schedule %>% dplyr::mutate(
        away_score_p =
          round(model$n.mean["LocationAway"] +
          model$n.ratings.offense[away] -
          model$n.ratings.defense[home], 1),
        home_score_p =
          round(model$n.mean["LocationHome"] +
          model$n.ratings.offense[home] -
          model$n.ratings.defense[away], 1),
        total_p =  round(home_score_p + away_score_p, 0),
        home_mov_p = round(home_score_p - away_score_p, 1))
    }

    # are there Poisson score ratings?
    if (!is.null(model$p.ratings.offense)) {
      aug_schedule <- aug_schedule %>% dplyr::mutate(
        away_score_p =
          round(exp(
            model$p.mean["LocationAway"] +
              model$p.ratings.offense[away] -
              model$p.ratings.defense[home]), 1),
        home_score_p =
          round(exp(
            model$p.mean["LocationHome"] +
              model$p.ratings.offense[home] -
              model$p.ratings.defense[away]), 1),
        total_p = round(home_score_p + away_score_p, 0),
        home_mov_p = round(home_score_p - away_score_p, 1))
    }

    # are there binomial win probability ratings?
    if (!is.null(model$b.ratings)) {
      aug_schedule <- aug_schedule %>% dplyr::mutate(
        home_prob_w =
          round(stats::pnorm(
            model$b.mean +
              model$b.ratings[home] -
              model$b.ratings[away]), 3),
        away_prob_w = round(1 - home_prob_w, 3),
        entropy = round(-log2(home_prob_w) * home_prob_w -
          log2(away_prob_w) * away_prob_w, 3))
    }

    return(aug_schedule)
  }

## wrappers for wrangling Kaggle NCAA data

#' @title Create a `game.data` tibble from Kaggle NCAA data
#' @name kaggle_game_data
#' @description `mvglmmRank` reads an input file in a specific `game.data`
#' format. This function takes a "compact results" dataset for an NCAA season
#' and adds columns to satisfy the `mvglmmRank` requirements.
#' @export kaggle_game_data
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom stats rbinom
#' @param compact_results a Kaggle NCAA "Compact Results" dataset
#' @return the dataset augmented with the input columns for mvglmmRank
#' @examples
#' \dontrun{
#' RegularSeasonCompactResults <- readr::read_csv(
#'   "~/DFS/kaggle/RegularSeasonCompactResults.csv",
#'   col_types = cols(
#'     DayNum = col_integer(),
#'     LScore = col_integer(),
#'     LTeamID = col_integer(),
#'     NumOT = col_integer(),
#'     Season = col_integer(),
#'     WScore = col_integer(),
#'     WTeamID = col_integer())) %>%
#'   filter(Season == 2019)
#' }

kaggle_game_data <- function(compact_results) {

  # split input rows into neutral and non-neutral sites
  not_neut <- compact_results %>%
    dplyr::filter(WLoc != "N") %>%
    dplyr::mutate(neutral.site = 0)
  is_neut <- compact_results %>%
    dplyr::filter(WLoc == "N") %>%
    dplyr::mutate(neutral.site = 1)

  # we need to randomly assign home and away for neutral sites
  coins <- rbinom(nrow(is_neut), size = 1, prob = 0.5)
  is_neut %<>% dplyr::mutate(
    WLocX = ifelse(coins == 1, "H", "A")
  )
  not_neut %<>% dplyr::mutate(
    WLocX = WLoc
  )
  # WLocX now has the computed home / away location for the winner

  # bind rows and finish
  game.data <- dplyr::bind_rows(not_neut, is_neut)
  game.data$neutral.site <- as.integer(game.data$neutral.site)

  # team "names"
  game.data %<>% dplyr::mutate(
    home = ifelse(WLocX == "H", as.character(WTeamID), as.character(LTeamID)))
  game.data %<>% dplyr::mutate(
    away = ifelse(WLocX == "A", as.character(WTeamID), as.character(LTeamID)))

  # team "responses" - we use scores
  game.data %<>% dplyr::mutate(
    home.response = ifelse(WLocX == "H", WScore, LScore))
  game.data %<>% dplyr::mutate(
    away.response = ifelse(WLocX == "A", WScore, LScore))

  # number of overtimes
  game.data %<>% dplyr::mutate(
    OT = NumOT)

  # binary response = 1 if home won, 0 if home lost
  game.data %<>% dplyr::mutate(
    binary.response = as.integer(home.response > away.response)
  )
  return(game.data)
}

#' @title Kaggle ratings
#' @name kaggle_ratings
#' @description Compute a team ratings tibble from an mvglmmRank model. The
#' model must have a `b.ratings` object - the method used must have "B" in
#' it.
#' @export kaggle_ratings
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom tibble enframe
#' @importFrom dplyr left_join
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @param model an `mvglmmRank` model object with a binary ratings object
#' @param teams a table of team IDs and names
#' @return a ratings tibble

kaggle_ratings <- function(model, teams) {
  ratings <- tibble::enframe(model$b.ratings)
  colnames(ratings) <- c("TeamID", "Rating")
  ratings$TeamID <- as.integer(ratings$TeamID)
  ratings %<>%  dplyr::left_join(teams, by = "TeamID") %>%
    dplyr::arrange(desc(Rating)) %>%
    dplyr::select(TeamID, "Team Name" = TeamName, Rating)
  return(ratings)
}

#' @title Get probability for a Kaggle "game_id"
#' @name kaggle_probability
#' @description Parses the probability from a `game.pred` result
#' @export kaggle_probability
#' @importFrom magrittr %>%
#' @importFrom utils capture.output
#' @importFrom stringr str_split
#' @importFrom mvglmmRank game.pred
#' @param model a model returned by `mvglmmRank_model`
#' @param game_id a Kaggle "game_id" - season, team 1 ID and team 2 ID
#' separated by underscores
#' @return the probability of a win for team 1

kaggle_probability <- function(model, game_id) {
  id_split <- stringr::str_split(game_id, pattern = "_")
  home <- id_split[[1]][2]
  away <- id_split[[1]][3]
  probability <- capture.output(mvglmmRank::game.pred(
    model, home, away, neutral.site = TRUE)) %>%
    grep(pattern = "Probability", value = TRUE) %>%
    sub(pattern = "^.*: ", replacement = "") %>%
    as.numeric()
  probability <- min(probability, 0.975) # nobody's *that* good!
  probability <- max(probability, 0.025) # nobody's *that* bad either!
}

## global name declarations
## See <https://github.com/STAT545-UBC/Discussion/issues/451#issuecomment-264598618>
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
  "away_prob_w",
  "away_score_p",
  "home_prob_w",
  "home_score_p",
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
  "home.response",
  "Season",
  "neutral.site",
  "TeamID",
  "TeamName",
  "desc",
  "value",
  "Rating"))
