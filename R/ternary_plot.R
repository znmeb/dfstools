#' @title Ternary Plot
#' @name ternary_plot
#' @description a visualization of a set of players using `ggtern`
#' @export ternary_plot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggtern aes
#' @importFrom ggtern ggtern
#' @importFrom ggtern theme_nomask
#' @importFrom RColorBrewer brewer.pal
#' @param player_alphas a data frame with player archetype values. The player
#' name must be in column `player_name` and the position in column `position`.
#' There must be exactly three archetypes, in order `Rim`, `Floor` and `Bench`.
#' @param plot_title the plot title
#' @return a `ggplot` object

ternary_plot <- function(player_alphas, plot_title) {

  # colour-blind-friendly palette
  cbPalette <- RColorBrewer::brewer.pal(n = 12, name = "Paired")
  xdata <- dplyr::mutate(
    player_alphas,
    `Player/Position` = paste(player_name, position, sep = "/")) %>%
    dplyr::arrange(Bench)
  plot_object <- ggtern(data = xdata, mapping = aes(Rim, Floor, Bench)) +
    geom_point(aes(
      shape = `Player/Position`,
      colour = `Player/Position`),
      size = 7.5) +
    theme_nomask() +
    scale_colour_manual(values = cbPalette) +
    scale_shape_manual(values = c(1:12)) +
    ggtitle(plot_title) +
    labs(x = "Rim", y = "Floor", z = "Bench")
    return(plot_object)
}

utils::globalVariables(c(
  "Bench",
  "Floor",
  "Player/Position",
  "position",
  "Rim"
))
