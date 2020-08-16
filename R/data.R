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

#' WNBA 2020 Player Totals After Nine Games
#'
#' A dataset containing the player total statistics for the first nine games of
#' the WNBA 2020 season. This is used as a test / sample dataset for the
#' archetypal analysis functions.
#'
#' @format A data frame with 144 rows and 27 variables:
#' \describe{
#'   \item{player}{player name}
#'   \item{tm}{team code name}
#'   \item{pos}{position(s)}
#'   \item{g}{games played}
#'   \item{gs}{games started}
#'   \item{mp}{minutes played}
#'   \item{fg}{field goals made}
#'   \item{fga}{field goals attempted}
#'   \item{fg_percent}{field goal percentage}
#'   \item{x3p}{three-point field goals made}
#'   \item{x3pa}{three-point field goals attempted}
#'   \item{x3p_percent}{three-point percentage}
#'   \item{x2p}{two-point field goals made}
#'   \item{x2pa}{two-point field goals attempted}
#'   \item{x2p_percent}{two-point field goal percentage}
#'   \item{e_fg_percent}{effective field goal percentage}
#'   \item{ft}{free throws made}
#'   \item{fta}{free throws attempted}
#'   \item{ft_percent}{free throw percentage}
#'   \item{orb}{offensive rebounds}
#'   \item{trb}{total rebounds - offensive plus defensive}
#'   \item{ast}{assists}
#'   \item{stl}{steals}
#'   \item{blk}{blocks}
#'   \item{tov}{turnovers}
#'   \item{pf}{personal fouls}
#'   \item{pts}{points}
#' }
#'
#' @source \url{https://www.basketball-reference.com/wnba/years/2020_totals.html}
"wnba_2020_first_nine_games_player_totals"

#' WNBA 2020 table of conferences
#'
#' A dataset with the list of conferences / teams
#'
#' @format A data frame with 12 rows and 3 variables:
#' \describe{
#'   \item{conference}{conference name}
#'   \item{team}{team name}
#'   \item{code}{team code name}
#' }
#'
#' @source \url{https://www.basketball-reference.com/wnba/}
"conference_table"
