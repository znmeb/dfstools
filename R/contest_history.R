#' @title Read DraftKings Contest History
#' @name read_draftkings_contest_history
#' @description Read a DraftKings contest history CSV into a tibble
#' @export read_draftkings_contest_history
#' @import readr
#' @param file CSV history file from DraftKings
#' @return a tibble of the contest history
#' @examples
#' \dontrun{
#' View(dfstools::read_draftkings_contest_history(
#' "~/Projects/draftkings/contest-standings/draftkings-contest-entry-history.csv"))
#' }

read_draftkings_contest_history <- function(file) {
	df <- read_csv(file, col_types = cols(
    Contest_Date_EST = col_character(),
    Entry_Fee = col_number(),
    Prize_Pool = col_number(),
    Winnings_Non_Ticket = col_number(),
    Winnings_Ticket = col_number()))

	# make database-friendly column names
	colnames(df) <- tolower(colnames(df))

	# convert the contest start time
	df$contest_date_est <- lubridate::ymd_hms(df$contest_date_est, tz = "EST5EDT")
	return(df)
}
