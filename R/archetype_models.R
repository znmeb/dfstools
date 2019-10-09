# all-zero columns crash the archetype algorithm
.is_valid_column <- function(x) {
  if (!is.numeric(x)) {
    TRUE
  } else {
    sum(as.double(x) * as.double(x)) > 0
  }
}

#  Text feet and inches to feet and decimals
.feet_inches_to_ft <- function(height) {
  feet <- as.numeric(sub("\'.*$", "", height))
  inches <- sub("^.*\'", "", height)
  inches <- as.numeric(sub("\".*$", "", inches))
  return(feet + inches / 12.0)
}

# utility function to do common processing after an archetype model has been run
.wrangle_archetype_results <- function(
  player_labels, input_matrix, archetype_model) {

  # get the archetype_parameters
  archetype_parameters <- t(archetypes::parameters(archetype_model))

  # compute the ordering
  ordering <- order(-archetype_parameters[1, ])

  # get the player_alphas
  raw_player_alphas <- archetype_model[["alphas"]]
  player_alphas <- raw_player_alphas
  for (i in 1:ncol(player_alphas)) {
    player_alphas[, i] <- scales::rescale(player_alphas[, i], to = c(0, 1))
  }
  rownames(player_alphas) <- rownames(input_matrix)

  # reorder the columns
  archetype_parameters <- archetype_parameters[, ordering]
  player_alphas <- player_alphas[, ordering]

  # use archetypal players for column names
  if (ncol(player_alphas) == 3) { # default case
    name_vector <- c("Rim", "Floor", "Bench")
  } else { # use the best players for column names
    name_vector <- c()
    for (i in 1:ncol(player_alphas)) {
      name_vector <- c(name_vector, names(which.max(player_alphas[, i])))
    }
    name_vector[ncol(player_alphas)] <- "Bench"
  }
  colnames(archetype_parameters) <- name_vector
  colnames(player_alphas) <- name_vector

  # make tibbles
  player_alphas <- player_alphas %>% as.data.frame() %>%
    tibble::rownames_to_column(var = "player_name") %>% as_tibble()
  player_alphas <- dplyr::left_join(player_labels, player_alphas) %>%
    arrange(Bench)
  archetype_parameters <- archetype_parameters %>% as.data.frame() %>%
    tibble::rownames_to_column(var = "statistic") %>% as_tibble()
  return(list(
    player_alphas = player_alphas,
    archetype_parameters = archetype_parameters
  ))

}

# all-zero columns crash the archetype algorithm
.is_valid_column <- function(x) {
  if (!is.numeric(x)) {
    TRUE
  } else {
    sum(as.double(x) * as.double(x)) > 0
  }
}

#' @title Archetype Search
#' @name archetype_search
#' @description stepwise search of archetype counts
#' @importFrom archetypes stepArchetypes
#' @importFrom archetypes bestModel
#' @importFrom archetypes robustArchetypes
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr select_if
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom tibble as_tibble
#' @importFrom scales rescale
#' @export archetype_search
#' @param player_totals a tibble of the values to use for archetypal analysis.
#' The first column must be the player name, which must match the player name
#' in the player_labels. The archetypes will be sorted on the second columns.
#' @param player_labels a tibble with the labels (player and team names,
#' positions, etc.). They may overlap with the totals. The leftmost column must
#' be the player name, which must match the player name in `player_totals`..
#' @param num_steps number of steps to use (default 1:5)
#' @param nrep number of repetitions at each step (default 4)
#' @param verbose should the search be verbose? (default FALSE)
#' @return a list of
#' \itemize{
#' \item archetype_parameters the parameters that define each archetype
#' \item player_alphas the players tagged with their loadings on each archetype
#' \item archetype_model the model object - the `bestModel` with `num_steps`
#' archetypes
#' \item all of the models}

archetype_search <- function(player_totals, player_labels,
                             num_steps = 1:7, nrep = 32, verbose = FALSE) {

  input_matrix <- player_totals %>%
    dplyr::select_if(.predicate = .is_valid_column) %>%
    tibble::column_to_rownames(var = "player_name") %>%
    as.matrix()
  set.seed(1776)
  archetype_models <- archetypes::stepArchetypes(
    data = input_matrix,
    k = num_steps,
    nrep = nrep,
    method = robustArchetypes,
    verbose = verbose
  )
  archetype_model <- archetypes::bestModel(archetype_models[max(num_steps)])

  # wrangle the results
  wrangled <-
    .wrangle_archetype_results(player_labels, input_matrix, archetype_model)

  return(list(
    archetype_parameters = wrangled$archetype_parameters,
    player_alphas = wrangled$player_alphas,
    archetype_model = archetype_model,
    archetype_models = archetype_models))
}

#' @title Archetypal Analysis
#' @name compute_archetypes
#' @description perform an "archetypal athletes" analysis
#' @importFrom archetypes archetypes
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr select_if
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom tibble as_tibble
#' @importFrom scales rescale
#' @export compute_archetypes
#' @param player_totals a tibble of the values to use for archetypal analysis.
#' The first column must be the player name, which must match the player name
#' in the player_labels. The archetypes will be sorted on the second columns.
#' @param player_labels a tibble with the labels (player and team names,
#' positions, etc.). They may overlap with the totals. The leftmost column must
#' be the player name, which must match the player name in `player_totals`..
#' @param num_archetypes number of archetypes to use (default 3)
#' @return a list of
#' \itemize{
#' \item archetype_parameters the parameters that define each archetype
#' \item player_alphas the players tagged with their loadings on each archetype
#' \item archetype_model the model object}

compute_archetypes <- function(player_totals, player_labels,
                               num_archetypes = 3) {

  input_matrix <- player_totals %>%
    dplyr::select_if(.predicate = .is_valid_column) %>%
    tibble::column_to_rownames(var = "player_name") %>%
    as.matrix()
  set.seed(1776)
  archetype_model <- archetypes(
    data = input_matrix,
    k = num_archetypes,
    verbose = FALSE
  )

  # wrangle the results
  wrangled <-
    .wrangle_archetype_results(player_labels, input_matrix, archetype_model)

  return(list(
    archetype_parameters = wrangled$archetype_parameters,
    player_alphas = wrangled$player_alphas,
    archetype_model = archetype_model))
}

utils::globalVariables(c(
  "games_played",
  "player_current_team_abbreviation",
  "player_height_ft",
  "player_labels",
  "player_rookie",
  "total_rebounds",
  "stats_defense_blk",
  "stats_defense_stl",
  "stats_defense_tov",
  "stats_field_goals_fg_2_pt_made",
  "stats_field_goals_fg_3_pt_made",
  "stats_free_throws_ft_made",
  "stats_games_played",
  "stats_offense_ast",
  "stats_rebounds_reb",
  "steals"
))
