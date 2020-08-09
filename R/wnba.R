#' @title Basketball Reference WNBA Schedule
#' @name wnba_2020_schedule_bbref
#' @description fetches the 2020 WNBA schedule from Basketball-reference.com
#' @export wnba_2020_schedule_bbref
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom lubridate mdy
#' @importFrom xml2 read_html
#' @return a tibble with three columns
#' \itemize{
#' \item `date` POSIXct the game date,
#' \item `away` character the "away" team
#' \item `home` character the "home" team
#' }
#' @examples
#' \dontrun{
#' wnba_schedule <- dfstools::wnba_2020_schedule_bbref()
#' View(wnba_schedule)
#' }

wnba_2020_schedule_bbref <- function() {
  df <- xml2::read_html(
      "https://www.basketball-reference.com/wnba/years/2020-schedule.html"
    ) %>% rvest::html_table()
  df[[1]] %>% dplyr::select(
    raw_date = "Date",
    away = "Visitor/Neutral",
    home = "Home/Neutral") %>%
  dplyr::mutate(
    date = lubridate::mdy(raw_date),
  )
}

#' @title Rest days
#' @name wnba_rest_days
#' @description Compute rest days from the schedule
#' @export wnba_rest_days
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_rows
#' @importFrom dplyr lag
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @param schedule a tibble returned by `wnba_2020_schedule_bbref`
#' @return a tibble with three columns
#' \itemize{
#' \item `team` character team name,
#' \item `date` POSIXct the game date,
#' \item `rest_days` numeric number of rest days
#' }
#' @examples
#' \dontrun{
#' wnba_schedule <- dfstools::wnba_2020_schedule_bbref()
#' View(wnba_schedule)
#' rest_days <- dfstools::wnba_rest_days(wnba_schedule)
#' View(rest_days)
#' }

wnba_rest_days <- function(schedule) {
  aways <- schedule %>% dplyr::select(team = away, date)
  homes <- schedule %>% dplyr::select(team = home, date)
  aways %>% dplyr::bind_rows(homes) %>%
    dplyr::arrange(team, date) %>%
    dplyr::group_by(team) %>%
    dplyr::mutate(rest_days = as.numeric(date - dplyr::lag(date)) - 1) %>%
    dplyr::ungroup()
}

## global name declarations - See
## https://github.com/STAT545-UBC/Discussion/issues/451#issuecomment-264598618
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
  "raw_date",
  "team"
))
