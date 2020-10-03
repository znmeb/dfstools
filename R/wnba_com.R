# internal functions for player totals
.fetch_player_totals <- function(season, season_type = "Regular+Season") {
  url <- paste0(
    "https://stats.wnba.com/stats/leagueLeaders?",
    "LeagueID=10&PerMode=Totals&Scope=S&Season=",
    season,
    "&SeasonType=",
    season_type,
    "&StatCategory=PTS"
  )
  json <- jsonlite::fromJSON(url, flatten = TRUE)
  raw_data <- as.data.frame(json[["resultSet"]][["rowSet"]])
  names(raw_data) <-
    janitor::make_clean_names(json[["resultSet"]][["headers"]])
  for (ixcol in 5:ncol(raw_data)) {
    numbers <- as.numeric(raw_data[, ixcol])
    numbers[is.na(numbers)] <- 0
    raw_data[, ixcol] <- numbers
  }
  return(raw_data)
}

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
#' arranged by descending minutes played
#' \item player_labels a tibble of labeling information for players
#' }
#' @examples
#' \dontrun{
#' wnba_2020_totals_wnba <- dfstools::wnba_season_totals_wnba(2020)
#' archetype_search_result <-
#'   dfstools::archetype_search(wnba_2020_totals_wnba$player_totals)
#' screeplot(archetype_search_result$archetype_models)
#' the_archetypes <-
#'   dfstools::compute_archetypes(wnba_2020_totals_wnba$player_totals)
#' View(the_archetypes$player_alphas)
#' View(the_archetypes$archetype_parameters)
#' }

wnba_season_totals_wnba <- function(season) {
  if (season < 1997 | season > 2020) {
    stop(paste("season", season, "is invalid!"))
  }

  raw_data <- .fetch_player_totals(season)
  raw_data <- raw_data %>% dplyr::mutate(
    player_name = paste(player, team),
    fg2a = fga - fg3a,
    fg2m = fgm - fg3m,
    negtov = -tov
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
    "fg2m",
    "fg3m",
    "ftm",
    "oreb",
    "dreb",
    "ast",
    "negtov",
    "stl",
    "blk"
  )
  player_totals <- raw_data %>%
    dplyr::select(stats_columns) %>%
    dplyr::arrange(desc(min)) %>%
    unique()

  return(list(
    player_totals = player_totals,
    player_labels = player_labels)
  )

}

# internal - team code table
.team_code_table <- tibble::tribble(
  ~team_name, ~team,
  "Atlanta Dream", "ATL",
  "Chicago Sky", "CHI",
  "Connecticut Sun", "CON",
  "Dallas Wings", "DAL",
  "Indiana Fever", "IND",
  "Las Vegas Aces", "LVA",
  "Los Angeles Sparks", "LAS",
  "Minnesota Lynx", "MIN",
  "New York Liberty", "NYL",
  "Phoenix Mercury", "PHO",
  "Seattle Storm", "SEA",
  "Washington Mystics", "WAS",
)

#' @title WNBA 2020 rank prep from wnba.com
#' @name wnba_2020_rank_prep_wnba
#' @description fetches the 2020 WNBA schedule from stats.wnba.com and
#' converts it to tables usable by `dfstools::mvglmmRank_model` and
#' `dfstools::game_predict`
#' @export wnba_2020_rank_prep_wnba
#' @importFrom dplyr %>%
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @return a list with three items
#' \itemize{
#' \item game.data a `game.data` table
#' \item games_played a table with the number of games each team has played
#' \item schedule a `schedule` table
#' }

wnba_2020_rank_prep_wnba <- function() {
  raw_data <- jsonlite::fromJSON(
    "https://data.wnba.com/data/5s/v2015/json/mobile_teams/wnba/2020/league/10_full_schedule.json",
    flatten = TRUE
  )
  schedule <- dplyr::bind_rows(
    raw_data[["lscd"]][["mscd.g"]][[1]],
    raw_data[["lscd"]][["mscd.g"]][[2]],
    raw_data[["lscd"]][["mscd.g"]][[3]],
    raw_data[["lscd"]][["mscd.g"]][[4]]
  ) %>% dplyr::mutate(
    date = gdte,
    away = paste(v.tc, v.tn, sep = " "),
    away.response = as.integer(v.s),
    home = paste(h.tc, h.tn, sep = " "),
    home.response = as.integer(h.s),
    neutral.site = 0,
    binary.response = as.integer(home.response > away.response)
  ) %>% dplyr::select(date:binary.response)
  game.data <- schedule %>% dplyr::filter(!is.na(away.response))

  home_games <- game.data %>%
    dplyr::group_by(home) %>%
    dplyr::summarise(home_games = dplyr::n())
  away_games <- game.data %>%
    dplyr::group_by(away) %>%
    dplyr::summarise(away_games = dplyr::n())
  games_played <-
    dplyr::full_join(away_games, home_games, by = c("away" = "home")) %>%
    dplyr::mutate(team_games_played = away_games + home_games) %>%
    dplyr::select(team_name = away, team_games_played) %>%
    dplyr::left_join(.team_code_table, by = "team_name")

  schedule <- schedule %>% dplyr::filter(is.na(away.response)) %>%
    dplyr::select(-away.response, -home.response, -binary.response)

  return(list(
    game.data = game.data,
    games_played = games_played,
    schedule = schedule
  ))
}
