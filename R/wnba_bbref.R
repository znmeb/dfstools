#' @title WNBA season player totals from Basketball Reference
#' @name wnba_season_totals_bbref
#' @description fetches the WNBA player totals for a given season from
#' basketball-reference.com and prepares the raw data for archetypal
#' analysis.
#' @export wnba_season_totals_bbref
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
#' wnba_totals <- dfstools::wnba_season_totals_bbref(2020)
#' player_totals <- wnba_totals$player_totals
#' player_labels <- wnba_totals$player_labels
#' the_archetypes <- dfstools::wnba_archetypes(player_totals)
#' player_alphas <- the_archetypes[["player_alphas"]]
#' archetype_parameters <- the_archetypes[["archetype_parameters"]]
#' View(player_alphas)
#' View(archetype_parameters)
#' }

wnba_season_totals_bbref <- function(season) {
  if (season < 1997 | season > 2020) {
    stop(paste("season", season, "is invalid!"))
  }
  url <- paste0(
    "https://www.basketball-reference.com/wnba/years/",
    season,
    "_totals.html"
  )
  raw_data <- xml2::read_html(url) %>% rvest::html_table()
  raw_data <- raw_data[[1]]
  for (ixcol in 4:ncol(raw_data)) {
    numbers <- as.numeric(raw_data[, ixcol])
    numbers[is.na(numbers)] <- 0
    raw_data[, ixcol] <- numbers
  }
 raw_data <- raw_data %>%
    janitor::clean_names() %>%
    dplyr::filter(player != "Player") %>%
    dplyr::mutate(
      player_name = paste(player, tm),
      drb = trb - orb
    )

  label_columns <- c(
    "player_name",
    "tm",
    "pos"
  )
  player_labels <- raw_data %>%
    dplyr::select(label_columns) %>%
    dplyr::arrange(player_name) %>%
    unique()

  stats_columns <- c(
    "player_name",
    "mp",
    "g",
    "gs",
    "x2pa",
    "x2p",
    "x3pa",
    "x3p",
    "fga",
    "fg",
    "fta",
    "ft",
    "orb",
    "drb",
    "trb",
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

#' @title WNBA 2020 rank prep from basketball-reference.com
#' @name wnba_2020_rank_prep_bbref
#' @description fetches the 2020 WNBA schedule from basketball-reference.com and
#' converts it to tables usable by `dfstools::mvglmmRank_model` and
#' `dfstools::game_predict`
#' @export wnba_2020_rank_prep_bbref
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom lubridate mdy
#' @importFrom xml2 read_html
#' @return a list with two items
#' \itemize{
#' \item game.data a `game.data` table
#' \item schedule a `schedule` table
#' }

wnba_2020_rank_prep_bbref <- function() {
  raw_data <- xml2::read_html(
    "https://www.basketball-reference.com/wnba/years/2020-schedule.html"
  ) %>% rvest::html_table()
  schedule <- raw_data[[1]]
  names(schedule) <- c(
    "raw_date",
    "away",
    "away.response",
    "home",
    "home.response",
    "discard"
  )
  schedule <- schedule %>% dplyr::mutate(
    date = lubridate::mdy(raw_date),
    neutral.site = 0,
    binary.response = as.integer(home.response > away.response)
  ) %>%
    dplyr::select(date, away:home.response, neutral.site:binary.response)
  game.data <- schedule %>% dplyr::filter(!is.na(away.response))
  schedule <- schedule %>% dplyr::filter(is.na(away.response)) %>%
    dplyr::select(-away.response, -home.response, -binary.response)
  return(list(game.data = game.data, schedule = schedule))
}
