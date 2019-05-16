#' @title Archetypoid Search
#' @name archetypoid_search
#' @description stepwise search of archetypoid counts
#' @importFrom Anthropometry stepArchetypesRawData
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom tibble as_tibble
#' @importFrom scales rescale
#' @export archetypoid_search
#' @param player_totals a tibble of the values to use for archetypal analysis.
#' The first column must be the player name, which must match the player name
#' in the player_labels. The archetypoids will be sorted on the second columns.
#' @param player_labels a tibble with the labels (player and team names,
#' positions, etc.). They may overlap with the totals. The leftmost column must
#' be the player name, which must match the player name in `player_totals`..
#' @param num_steps number of steps to use (default 1:5)
#' @param nrep number of repetitions at each step (default 4)
#' @param verbose should the search be verbose? (default FALSE)
#' @return a list of
#' \itemize{
#' \item archetypoid_parameters the parameters that define each archetypoid
#' \item player_alphas the players tagged with their loadings on each archetypoid
#' \item archetypoid_model the model object - the `bestModel` with `num_steps`
#' archetypoids
#' \item all of the models}

archetypoid_search <- function(player_totals, player_labels,
                             num_steps = 1:15, nrep = 8, verbose = FALSE) {

  input_matrix <- player_totals %>%
    tibble::column_to_rownames(var = "player_name")
  preprocessed_data <- Anthropometry::preprocessing(
    input_matrix, stand = TRUE, percAccomm = 1
  )
  set.seed(1776)
  archetypoid_models <- Anthropometry::stepArchetypesRawData(
    data = preprocessed_data$data,
    numArch = num_steps,
    numRep = nrep,
    verbose = verbose
  )
  return(list(
    archetypoid_models = archetypoid_models,
    preprocessed_data = preprocessed_data
  ))
}
#   archetypoid_model <- Anthropometry::bestModel(archetypoid_models[max(num_steps)])
#
#   # wrangle the results
#   wrangled <-
#     .wrangle_archetypoid_results(player_labels, input_matrix, archetypoid_model)
#
#   return(list(
#     archetypoid_parameters = wrangled$archetypoid_parameters,
#     player_alphas = wrangled$player_alphas,
#     archetypoid_model = archetypoid_model,
#     archetypoid_models = archetypoid_models))
# }

utils::globalVariables(c(
))
