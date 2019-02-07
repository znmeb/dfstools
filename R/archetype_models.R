#' @title NBA Archetypal Analysis
#' @name nba_archetypes
#' @description perform an "archetypal athletes" analysis
#' @importFrom archetypes archetypes
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom tibble as_tibble
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
#' dfstools::msf_set_apikey("your MySportsFeeds API key")
#' player_totals <- dfstools::nba_player_season_totals("current")
#' the_archetypes <- dfstools::nba_archetypes(player_totals)
#' player_alphas <- the_archetypes[["player_alphas"]]
#' View(player_alphas)
#' }

nba_archetypes <- function(player_totals, num_archetypes = 3) {

  player_labels <- player_totals %>% dplyr::select(
    player_name:player_current_team_abbreviation,
    player_height,
    player_height_ft,
    player_rookie
  )
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
  ordering <- order(-archetype_parameters["stats_minutes_played", ])

  # get the player_alphas
  player_alphas <- archetype_model[["alphas"]]
  rownames(player_alphas) <- rownames(input_matrix)

  # reorder the columns
  archetype_parameters <- archetype_parameters[, ordering]
  player_alphas <- player_alphas[, ordering]

  # use archetypal players for column names
  name_vector <- c()
  for (i in 1:ncol(player_alphas)) {
    name_vector <- c(name_vector, names(which.max(player_alphas[, i])))
  }
  colnames(archetype_parameters) <- name_vector
  colnames(player_alphas) <- name_vector

  # make tibbles
  player_alphas <- player_alphas %>% as.data.frame() %>%
    tibble::rownames_to_column(var = "player_name") %>% as_tibble()
  player_alphas <- dplyr::left_join(player_labels, player_alphas)
  archetype_parameters <- archetype_parameters %>% as.data.frame() %>%
    tibble::rownames_to_column(var = "statistic") %>% as_tibble()

  return(list(
    archetype_parameters = archetype_parameters,
    player_alphas = player_alphas,
    archetype_model = archetype_model))
}

#' @title NBA Archetype Search
#' @name nba_archetype_search
#' @description stepwise search of archetype counts
#' @importFrom archetypes stepArchetypes
#' @importFrom archetypes bestModel
#' @importFrom archetypes robustArchetypes
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom tibble as_tibble
#' @export nba_archetype_search
#' @param player_totals a tibble returned by `nba_player_season_totals`
#' @param num_steps number of steps to use (default 7)
#' @return a list of
#' \itemize{
#' \item archetype_parameters the parameters that define each archetype
#' \item player_alphas the players tagged with their loadings on each archetype
#' \item archetype_model the model object - the `bestModel` with `num_steps`
#' archetypes
#' \item all of the models}
#' @examples
#' \dontrun{
#' dfstools::msf_set_apikey("your MySportsFeeds API key")
#' player_totals <- dfstools::nba_player_season_totals("current")
#' the_archetypes <- dfstools::nba_archetype_search(player_totals)
#' player_alphas <- the_archetypes[["player_alphas"]]
#' View(player_alphas)
#' }


nba_archetype_search <- function(player_totals, num_steps = 7) {

  player_labels <- player_totals %>% dplyr::select(
    player_name:player_current_team_abbreviation,
    player_height,
    player_height_ft,
    player_rookie
  )
  input_matrix <- player_totals %>%
    dplyr::select(
      player_name, stats_minutes_played:stats_miscellaneous_fouls
    ) %>%
    tibble::column_to_rownames(var = "player_name") %>%
    as.matrix()
  set.seed(1776)
  archetype_models <- archetypes::stepArchetypes(
    data = input_matrix,
    k = 1:num_steps,
    nrep = 16,
    method = robustArchetypes,
    verbose = TRUE
  )
  archetype_model <- archetypes::bestModel(archetype_models[num_steps])

  # get the archetype_parameters
  archetype_parameters <- t(archetypes::parameters(archetype_model))

  # compute the ordering
  ordering <- order(-archetype_parameters["stats_minutes_played", ])

  # get the player_alphas
  player_alphas <- archetype_model[["alphas"]]
  rownames(player_alphas) <- rownames(input_matrix)

  # reorder the columns
  archetype_parameters <- archetype_parameters[, ordering]
  player_alphas <- player_alphas[, ordering]

  # use archetypal players for column names
  name_vector <- c()
  for (i in 1:ncol(player_alphas)) {
    name_vector <- c(name_vector, names(which.max(player_alphas[, i])))
  }
  colnames(archetype_parameters) <- name_vector
  colnames(player_alphas) <- name_vector

  # make tibbles
  player_alphas <- player_alphas %>% as.data.frame() %>%
    tibble::rownames_to_column(var = "player_name") %>% as_tibble()
  player_alphas <- dplyr::left_join(player_labels, player_alphas)
  archetype_parameters <- archetype_parameters %>% as.data.frame() %>%
    tibble::rownames_to_column(var = "statistic") %>% as_tibble()

  return(list(
    archetype_parameters = archetype_parameters,
    player_alphas = player_alphas,
    archetype_model = archetype_model,
    archetype_models = archetype_models))
}

utils::globalVariables(c(
  "player_current_team_abbreviation",
  "player_height_ft",
  "player_rookie"
))
