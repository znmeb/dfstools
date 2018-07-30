# internal function to select_if non-zero columns
.any_non_zero <- function(x) {
  any(x != 0)
}

## See <https://github.com/STAT545-UBC/Discussion/issues/451#issuecomment-264598618>
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
  "player_full_name",
  "Back",
  "Bench",
  "Front",
  "own_team",
  "Player",
  "Player/Position",
  "position"
))

# internal function to make archetype analysis input
.archetype_prep <- function(pbs) {

  # figure out which team each player is currently on
  current_team <- pbs %>%
    dplyr::group_by(player_full_name) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::select(player_full_name, position, own_team) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(player_full_name)

  # compute the data matrix
  leftmost <- 8 # leftmost numeric in a box score row
  rightmost <- ncol(pbs) # rightmost numeric
  pbs_summary <- pbs %>%
    dplyr::select(player_full_name, leftmost:rightmost) %>%
    dplyr::select(-ddbl:-tdbl, -fdfp:-yfp) %>%
    dplyr::group_by(player_full_name) %>%
    dplyr::summarize_if(is.numeric, sum, na.rm = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::select_if(.any_non_zero) %>%
    dplyr::filter(gmsc > 0) %>%
    dplyr::mutate(avg_gmsc = gmsc / games) %>%
    dplyr::arrange(player_full_name) %>%
    dplyr::select(player_full_name, `min`:`avg_gmsc`)
  pbs_summary <-
    dplyr::inner_join(current_team, pbs_summary, by = "player_full_name")
  data_matrix <- pbs_summary %>%
    dplyr::select(`min`:`pts`) %>%
    data.matrix()
  return(list(data_matrix = data_matrix, pbs_summary = pbs_summary))
}

#' @title Archetype Search
#' @name archetype_search
#' @description search for the best archetypal analysis of real box score values
#' @export archetype_search
#' @param pbs player box score
#' @return an "as" object from archetypes::stepArchetypes

archetype_search <- function(pbs) {

  # make input dataset
  arch_input <- .archetype_prep(pbs)

  # run the archetypal analysis
  max_arch <- 5
  arch_data <- arch_input$data_matrix
  set.seed(1924)
  arch_results <- archetypes::stepArchetypes(
    data = arch_data,
    method = archetypes::robustArchetypes,
    k = 1:max_arch,
    verbose = FALSE,
    nrep = 64)
  return(list(arch_input = arch_input, arch_results = arch_results))
}

#' @title Ternary Plot
#' @name ternary_plot
#' @description a visualization of a set of players using `ggtern`
#' @export ternary_plot
#' @import ggtern
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 ggtitle
#' @param player_table a data frame with player archetype values
#' @param plot_title the plot title
#' @return a `ggplot` object

ternary_plot <- function(player_table, plot_title, ix_col) {

  # colour-blind-friendly palette
  cbPalette <- c(
    RColorBrewer::brewer.pal(n = 8, name = "Dark2"),
    RColorBrewer::brewer.pal(n = 8, name = "Dark2")
  )
  xdata <- dplyr::mutate(
    player_table,
    `Player/Position` = paste(Player, Position, sep = "/")) %>%
    dplyr::arrange(desc(TotGmSc))
  xdata$`Player/Position` <- forcats::as_factor(xdata$`Player/Position`)
  plot_object <- ggtern(
    data = xdata, mapping = aes(
      x = xdata[, ix_col],
      y = xdata[, ix_col + 1],
      z = xdata[, ix_col + 2])) +
    geom_point(aes(
      shape = `Player/Position`,
      colour = `Player/Position`),
      size = 7.5) +
    theme_nomask() +
    scale_colour_manual(values = cbPalette) +
    scale_shape_manual(values = c(1:12)) +
    ggtitle(plot_title) +
    Llab(colnames(xdata)[ix_col]) +
    Tlab(colnames(xdata)[ix_col + 1]) +
    Rlab(colnames(xdata)[ix_col + 2])
    return(plot_object)
}
