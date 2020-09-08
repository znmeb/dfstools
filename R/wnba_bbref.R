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
