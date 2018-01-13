## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------
#  library(tidysportsfeeds)
#  stattleshipR::set_token(token)
#  nba_games <- get_games("nba")
#  nba_result <- project_upcoming_games(nba_games)
#  nba_projections <- nba_result$projections
#  readr::write_excel_csv(nba_projections, "nba_projections.csv")

