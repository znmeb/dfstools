#' @title WNBA season player totals from wnba.com
#' @name wnba_season_totals_wnba
#' @description fetches the WNBA player totals for a given season from
#' wnba.com and prepares the raw data for archetypal
#' analysis.
#' @export wnba_season_totals_wnba
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom tibble remove_rownames
#' @importFrom janitor clean_names
#' @importFrom rvest html_table
#' @importFrom xml2 read_html
#' @param season season to fetch (1997 - 2020)
#' @return a list of two items
#' \itemize{
#' \item player_totals a tibble of player season total box score statistics,
#' arranged by descending points scored
#' \item player_labels a tibble of labeling information for players
#' }
#' @examples
#' \dontrun{
#' wnba_totals <- dfstools::wnba_season_totals_wnba(2020)
#' player_totals <- wnba_totals$player_totals
#' player_labels <- wnba_totals$player_labels
#' the_archetypes <- dfstools::wnba_archetypes(player_totals)
#' player_alphas <- the_archetypes[["player_alphas"]]
#' archetype_parameters <- the_archetypes[["archetype_parameters"]]
#' View(player_alphas)
#' View(archetype_parameters)
#' }

wnba_season_totals_wnba <- function(season) {
  if (season < 1997 | season > 2020) {
    stop(paste("season", season, "is invalid!"))
  }
  url <- paste0(
    "https://stats.wnba.com/stats/leagueLeaders?LeagueID=10&PerMode=Totals&Scope=S&Season=",
    season,
    "&SeasonType=Regular+Season&StatCategory=PTS"
  )
  json <- jsonlite::fromJSON(url, flatten = TRUE)
  raw_data <- as.data.frame(json[["resultSet"]][["rowSet"]])
  names(raw_data) <- janitor::make_clean_names(json[["resultSet"]][["headers"]])
  for (ixcol in 5:ncol(raw_data)) {
    numbers <- as.numeric(raw_data[, ixcol])
    numbers[is.na(numbers)] <- 0
    raw_data[, ixcol] <- numbers
  }
  raw_data <- raw_data %>% dplyr::mutate(
    player_name = paste(player, team),
    fg2a = fga - fg3a,
    fg2m = fgm - fg3m
  )

  label_columns <- c(
    "player_name",
    "team"
  )
  player_labels <- raw_data %>%
    dplyr::select(label_columns) %>%
    dplyr::arrange(player_name) %>%
    unique()

  stats_columns <- c(
    "player_name",
    "min",
    "gp",
    "fg2a",
    "fg2m",
    "fg3a",
    "fg3m",
    "fga",
    "fgm",
    "fta",
    "ftm",
    "oreb",
    "dreb",
    "reb",
    "ast",
    "pts",
    "tov",
    "stl",
    "blk",
    "pf"
  )
  player_totals <- raw_data %>%
    dplyr::select(stats_columns) %>%
    dplyr::arrange(desc(pts)) %>%
    unique()

  return(list(
    player_totals = player_totals,
    player_labels = player_labels)
  )

}

#' @title WNBA 2020 Schedule from WNBA site
#' @name wnba_2020_schedule_wnba
#' @description fetches the 2020 WNBA schedule from stats.wnba.com and
#' converts it to a format usable by dfstools::mvglmmRank_model
#' @export wnba_2020_schedule_wnba
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom jsonlite fromJSON
#' @return a tibble that can be truncated to serve as a `game.data` input for
#' `dfstools::mvglmmRank_model` and as a `schedule` input for
#' `dfstools::game_predict`.
#' @examples
#' \dontrun{
#' library(magrittr)
#' wnba_schedule <- dfstools::wnba_2020_schedule_wnba()
#' wnba_game_data <- dfstools::wnba_make_game_data(wnba_schedule)
#' wnba_game_predict_schedule <-
#'   dfstools::wnba_make_game_predict_schedule(wnba_schedule)
#' View(wnba_game_data)
#' View(wnba_game_predict_schedule)
#' wnba_model <- dfstools::mvglmmRank_model(wnba_game_data, verbose = FALSE)
#' teams <- as.character(names(wnba_model[["n.ratings.offense"]]))
#' ratings <- tibble::as_tibble(list(
#'   team = teams,
#'   offense = wnba_model[["n.ratings.offense"]],
#'   defense = wnba_model[["n.ratings.defense"]]
#' ))
#' forecast <- dfstools::game_predict(
#'   schedule = wnba_game_predict_schedule,
#'   model = wnba_model
#' )
#' forecast <- forecast %>%
#'   dplyr::select(-away.response, -home.response, -binary.response)
#' entropy <- forecast %>%
#'   dplyr::group_by(date) %>%
#'   dplyr::summarise(total_entropy = sum(entropy))
#' }

wnba_2020_schedule_wnba <- function() {
  raw_data <- fromJSON(
    "https://data.wnba.com/data/5s/v2015/json/mobile_teams/wnba/2020/league/10_full_schedule.json",
    flatten = TRUE
  )
  schedule <- dplyr::bind_rows(
    raw_data[["lscd"]][["mscd.g"]][[1]],
    raw_data[["lscd"]][["mscd.g"]][[2]],
    raw_data[["lscd"]][["mscd.g"]][[3]]
  ) %>% dplyr::mutate(
    date = gdte,
    away = paste(v.tc, v.tn, sep = " "),
    away.response = as.integer(v.s),
    home = paste(h.tc, h.tn, sep = " "),
    home.response = as.integer(h.s),
    neutral.site = 0,
    binary.response = as.integer(home.response > away.response)
  ) %>% dplyr::select(date:binary.response)
}

## global name declarations - See
## https://github.com/STAT545-UBC/Discussion/issues/451#issuecomment-264598618
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
))
