#' @title NBA Archetypal Analysis
#' @name nba_archetypes
#' @description perform an "archetypal athletes" analysis
#' @importFrom archetypes archetypes
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom tibble as_tibble
#' @importFrom scales rescale
#' @export nba_archetypes
#' @param player_totals a tibble returned by `nba_player_season_totals`
#' @param num_archetypes number of archetypes to use (default 3)
#' @return a list of
#' \itemize{
#' \item archetype_parameters the parameters that define each archetype
#' \item player_alphas the players tagged with their loadings on each archetype
#' \item archetype_model the model object}
#' @examples
#' \dontrun{
#' player_totals <- dfstools::nba_player_season_totals("current")
#' the_archtypes <- dfstools::nba_archetypes(player_totals)
#' )}

nba_archetypes <- function(player_totals, num_archetypes = 3) {

  input_matrix <- player_totals %>%
    dplyr::select(
      player_name, stats_minutes_played:stats_miscellaneous_fouls
    ) %>%
    tibble::column_to_rownames(var = "player_name") %>%
    as.matrix()
  set.seed(1776)
  archetype_model <- archetypes(
    data = input_matrix,
    k = num_archetypes,
    verbose = FALSE
  )

  # get the archetype_parameters
  archetype_parameters <- t(archetypes::parameters(archetype_model))

  # compute the ordering
  ordering <- order(-archetype_parameters["stats_rebounds_reb", ])

  # get the player_alphas
  player_alphas <- archetype_model[["alphas"]]
  rownames(player_alphas) <- rownames(input_matrix)

  # reorder the columns
  archetype_parameters <- archetype_parameters[, ordering]
  player_alphas <- player_alphas[, ordering]

  # column names
  colnames(archetype_parameters) <- colnames(player_alphas) <-
    c("rebounds", "threes", "bench")

  # make tibbles
  player_alphas <- player_alphas %>% as.data.frame() %>%
    tibble::rownames_to_column(var = "player_name") %>% as_tibble()
  archetype_parameters <- archetype_parameters %>% as.data.frame() %>%
    tibble::rownames_to_column(var = "statistic") %>% as_tibble()

  return(list(
    archetype_parameters = archetype_parameters,
    player_alphas = player_alphas,
    archetype_model = archetype_model))
}
