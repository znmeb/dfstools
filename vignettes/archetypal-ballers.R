## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE----------------------------------------------------------
# library(magrittr)
# library(dfstools)
# num_players <- 12 # number of players to display
# load("~/Projects/mastering-dfs-analytics-package/test-notebooks/ternary-data.RData")
# display_table <- function(table) {
#   knitr::kable(table, digits = 4)
# }
# player_table %<>% dplyr::filter(Bench < 0.99)


## ----echo=FALSE----------------------------------------------------------
# top_back <- player_table %>% 
#   dplyr::top_n(num_players, Back) %>% 
#   dplyr::arrange(desc(Back))
# (top_back %>% dplyr::select(-Bench)) %>% display_table()


## ----echo=FALSE----------------------------------------------------------
# top_front <- player_table %>% 
#   dplyr::top_n(num_players, Front) %>% 
#   dplyr::arrange(desc(Front))
# (top_front %>% dplyr::select(-Bench)) %>% display_table()

## ----echo=FALSE----------------------------------------------------------
# top_overall <- player_table %>% 
#   dplyr::top_n(num_players, Overall) %>% 
#   dplyr::arrange(desc(Overall))
# (top_overall %>% dplyr::select(-Bench)) %>% display_table()

## ----echo=FALSE----------------------------------------------------------
# ternary_plot(
#   player_table = top_back, 
#   plot_title = "NBA Top Twelve Backcourt", 
#   top_label = "Back", 
#   left_label = "Front") %>% print()

## ----echo=FALSE----------------------------------------------------------
# ternary_plot(
#   player_table = top_front, 
#   plot_title = "NBA Top Twelve Frontcourt", 
#   top_label = "Back", 
#   left_label = "Front") %>% print()

## ----echo=FALSE----------------------------------------------------------
# ternary_plot(
#   player_table = top_overall, 
#   plot_title = "NBA Top Twelve Overall", 
#   top_label = "Back", 
#   left_label = "Front") %>% print()

## ----echo=FALSE----------------------------------------------------------
# blazers <- player_table %>% 
#   dplyr::filter(Team == "Portland") %>% 
#   dplyr::top_n(num_players, Overall) %>% 
#   dplyr::arrange(desc(Overall))
# (blazers %>% dplyr::select(-Bench)) %>% display_table()

## ----echo=FALSE----------------------------------------------------------
# ternary_plot(
#   player_table = blazers, 
#   plot_title = "Portland Trail Blazers", 
#   top_label = "Back", 
#   left_label = "Front") %>% print()

