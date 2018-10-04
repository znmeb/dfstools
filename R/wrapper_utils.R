# names of teams for endpoints / queries
.team_slugs <- list()
.team_slugs[["nba"]] <- c(
  "nba-atl", "nba-bos", "nba-bk", "nba-cha", "nba-chi",
  "nba-cle", "nba-dal", "nba-den", "nba-det", "nba-gs",
  "nba-hou", "nba-ind", "nba-lac", "nba-lal", "nba-mem",
  "nba-mia", "nba-mil", "nba-min", "nba-no", "nba-ny",
  "nba-okc", "nba-orl", "nba-phi", "nba-pho", "nba-por",
  "nba-sac", "nba-sa", "nba-tor", "nba-uta", "nba-was")

.team_slugs[["nfl"]] <- c(
  "nfl-ari", "nfl-atl", "nfl-bal", "nfl-buf", "nfl-car",
  "nfl-chi", "nfl-cin", "nfl-cle", "nfl-dal", "nfl-den",
  "nfl-det", "nfl-gb", "nfl-hou", "nfl-ind", "nfl-jac",
  "nfl-kc", "nfl-lac", "nfl-stl", "nfl-mia", "nfl-min",
  "nfl-ne", "nfl-no", "nfl-nyj", "nfl-nyg", "nfl-oak",
  "nfl-phi", "nfl-pit", "nfl-sf", "nfl-sea", "nfl-tb",
  "nfl-ten", "nfl-was")

.team_slugs[["nhl"]] <- c(
  "nhl-ana", "nhl-ari", "nhl-bos", "nhl-buf", "nhl-cal",
  "nhl-car", "nhl-chi", "nhl-col", "nhl-clb", "nhl-dal",
  "nhl-det", "nhl-edm", "nhl-fla", "nhl-la", "nhl-min",
  "nhl-mon", "nhl-nas", "nhl-nj", "nhl-nyi", "nhl-nyr",
  "nhl-ott", "nhl-phi", "nhl-pit", "nhl-sj", "nhl-stl",
  "nhl-tb", "nhl-tor", "nhl-van", "nhl-vgk", "nhl-was",
  "nhl-win")

# internal function to look up sport name
.sport <- function(league) {
  sports <- c("basketball", "hockey", "football", "baseball")
  names(sports) <- c("nba", "nhl", "nfl", "mlb")
  return(sports[league])
}

#' @title Get MySportsFeed DFS data
#' @name get_mysportsfeeds_dfs
#' @description Gets DFS data object from the MySportsFeeds.com API
#' @export get_mysportsfeeds_dfs
#' @importFrom mysportsfeedsR msf_get_results
#' @importFrom tibble as_tibble
#' @param league ("nba", "nhl", "nfl" or "mlb")
#' @param season Look up season code in API docs https://www.mysportsfeeds.com/data-feeds/api-docs
#' @return a list of tibbles with the DFS data for the league and season. There is one tibble for each DFS site, currently "DraftKings" and "FanDuel".
#' @examples
#' \dontrun{
#' username <- "your_user_name"
#' password <- "your_password"
#' library(dfstools)
#' mysportsfeedsR::authenticate_v1_x(username, password)
#' nba_dfs_2017_2018 <-
#'   get_mysportsfeeds_dfs(league = "nba", season = "2017-2018-regular")
#' }

get_mysportsfeeds_dfs <- function(league, season) {
  res <- mysportsfeedsR::msf_get_results(
    league = league,
    season = season,
    feed = "daily_dfs",
    verbose = FALSE)
  sites <-
    res[["api_json"]][["dailydfs"]][["dfsEntries"]][["dfsType"]]
  return_list <- list()
  for (i in 1:length(sites)) {
    tbl_df <- tibble::as_tibble(
      res$api_json$dailydfs$dfsEntries$dfsRows[[i]])
    tbl_df$salary <- as.numeric(tbl_df$salary)
    tbl_df$fantasyPoints <- as.numeric(tbl_df$fantasyPoints)
    return_list[[sites[i]]] <- tbl_df
  }
  return(return_list)
}
