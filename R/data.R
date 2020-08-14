#' WNBA 2020 Schedule After Nine Games
#'
#' A dataset containing the scores of the first nine games of the WNBA 2020
#' season, followed by the schedule for the rest of the season. This is used
#' as a test / sample dataset for the `mvglmmRank` modeling and game
#' prediction functions.
#'
#' Note that all 2020 WNBA games are played in the "Wubble" in Bradenton,
#' Florida, ostensibly a neutral site. However, each team is assigned "home" or
#' "away" status, the game times are set so they make sense to the home team's
#' fans, and the background sounds and advertising displays mirror those a home
#' team viewer would hear and see. Somewhat surprisingly, the models I've run
#' so far indicate that there ***is*** a home-court advantage!
#'
#' @format A data frame with 132 rows and 7 variables:
#' \describe{
#'   \item{date}{game date}
#'   \item{away}{away team name}
#'   \item{away.response}{away team score}
#'   \item{home}{home team name}
#'   \item{home.response}{home team score}
#'   \item{neutral.site}{is the site neutral? 0=F, 1=T, all set to 0}
#'   \item{binary.response}{did the home team win? 0=F, 1=T}
#' }
#'
#' @source \url{https://www.basketball-reference.com/wnba/years/2020-schedule.html}
"wnba_2020_first_nine_games"
