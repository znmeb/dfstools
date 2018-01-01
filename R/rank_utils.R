## See <https://github.com/STAT545-UBC/Discussion/issues/451#issuecomment-264598618>
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
  "at_neutral_site",
  "away",
  "prob_away_wins",
  "away_score_proj",
  "away.response",
  "away_team_score",
  "ended_at",
  "entropy",
  "home",
  "home_mov_proj",
  "prob_home_wins",
  "home_score_proj",
  "home.response",
  "home_team_score",
  "label",
  "league",
  "method",
  "neutral.site",
  "neutral_site",
  "OT",
  "period",
  "ppg",
  "slug",
  "started_at",
  "scheduled_at",
  "status",
  "total_proj"
))

## internal function for periods per game for a league
.ppg <- function(league) {
  ifelse(league == "nhl", 3, ifelse(league == "mlb", 9, 4))
}

## internal function for model input data tibble
.extract_model_input_data <- function(raw_season) {
  raw_season %>% dplyr::mutate(
    league = sub("-.*$", "", slug),
    ppg = .ppg(league),
    away = sub(" vs .*$", "", label),
    home = sub("^.* vs ", "", label),
    away.response = away_team_score,
    home.response = home_team_score,
    binary.response = as.integer(home.response > away.response),
    neutral.site = ifelse(is.na(at_neutral_site), 0, 1),
    OT = ifelse(period > ppg, 1, 0)) %>%
    dplyr::arrange(started_at) %>%
    dplyr::select(away:OT, started_at, ended_at, periods = period, status)
}

#' @title Project upcoming games for the current season
#' @name project_upcoming_games
#' @description Creates a tibble with projections for the rest of the games in a season
#' @export project_upcoming_games
#' @importFrom dplyr %>%
#' @importFrom magrittr %<>%
#' @param raw_season a tibble returned from the Stattleship API via `get_season`
#' @return a list with three items:
#' \itemize{
#' \item projections: a tibble with the projections for the rest of the season.
#' \item model: the model that mvglmmRank built from the closed games
#' \item model_input: model input data from the closed games}
#' @examples
#' \dontrun{
#' token <- "yourtoken"
#' library(tidysportsfeeds)
#' library(stattleshipR)
#' stattleshipR::set_token(token)
#' nba_raw <-
#'   tidysportsfeeds::get_season(league = "nba")
#' nba_result <- project_upcoming_games(nba_raw)
#' nba_projections <- nba_result$projections
#' readr::write_excel_csv(nba_projections, "nba_projections.csv")
#' }

project_upcoming_games <- function(raw_season) {

  # get model input
  season_data <- .extract_model_input_data(raw_season)
  model_input <- season_data %>% dplyr::filter(status == "closed")
  projections <- season_data %>% dplyr::filter(status == "upcoming") %>%
    dplyr::select(away, home, neutral_site = neutral.site, scheduled_at = started_at)

  # build model
  model <- mvglmmRank::mvglmmRank(
    model_input, method = "PB1", first.order = FALSE, verbose = FALSE, OT.flag = TRUE)

  # add projection columns
  projections %<>% dplyr::mutate(method = model$method)

  # are there normal score ratings?
  if (!is.null(model$n.ratings.offense)) {
    projections %<>% dplyr::mutate(
      away_score_proj =
        model$n.mean["LocationAway"] +
        model$n.ratings.offense[away] -
        model$n.ratings.defense[home],
      home_score_proj =
        model$n.mean["LocationHome"] +
        model$n.ratings.offense[home] -
        model$n.ratings.defense[away])
    }

    # are there Poisson score ratings?
    if (!is.null(model$p.ratings.offense)) {
      projections %<>% dplyr::mutate(
        away_score_proj =
          exp(
            model$p.mean["LocationAway"] +
            model$p.ratings.offense[away] -
            model$p.ratings.defense[home]),
        home_score_proj =
          exp(
            model$p.mean["LocationHome"] +
            model$p.ratings.offense[home] -
            model$p.ratings.defense[away]))
    }

    # are there binomial win probability ratings?
    if (!is.null(model$b.ratings)) {
      projections %<>% dplyr::mutate(
        prob_home_wins =
          stats::pnorm(
            model$b.mean +
            model$b.ratings[home] -
            model$b.ratings[away]),
        prob_away_wins = 1 - prob_home_wins,
        entropy = -log2(prob_home_wins) * prob_home_wins -
          log2(prob_away_wins) * prob_away_wins)
    }

  projections %<>%
    dplyr::mutate(
      home_mov_proj = home_score_proj - away_score_proj,
      total_proj = home_score_proj + away_score_proj
    ) %>%
    dplyr::select(
      away, home,
      prob_away_wins, prob_home_wins,
      away_score_proj, home_score_proj,
      home_mov_proj, total_proj,
      entropy, scheduled_at, neutral_site, method)
  return(list(projections = projections, model = model, model_input = model_input))
}
