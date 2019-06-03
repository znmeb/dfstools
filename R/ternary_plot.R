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
#' name must be in column `Player`.
#' There must be exactly three archetypes, in order `Rim Protection`,
#' `Floor Spacing` and `Bench`.
#' @param plot_title the plot title
#' @return a `ggplot` object

ternary_plot <- function(player_alphas, plot_title) {

  x4 <- c("#FF0000", "#008000", "#0000FF", "#000000")
  palette <- c(x4, x4, x4)
  plot_object <- ggtern(
    data = player_alphas, mapping = aes(Rim, Floor, Bench)) +
    geom_point(aes(
      shape = Player,
      colour = Player),
      size = 5) +
    theme_nomask() +
    scale_colour_manual(values = palette) +
    scale_shape_manual(values = c(1:12)) +
    ggtitle(plot_title) +
    labs(x = "Rim", y = "Floor", z = "Bench")
    return(plot_object)
}

utils::globalVariables(c(
  "Bench",
  "Floor",
  "Player",
  "position",
  "Rim"
))
