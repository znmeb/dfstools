#! /usr/bin/env Rscript

CRAN <- "https://cloud.r-project.org/"
LIB <- Sys.getenv("R_LIBS_USER")
dir.create(LIB, recursive = TRUE, mode = '0755')
.libPaths(LIB)

update.packages(ask = FALSE, lib = LIB, repos = CRAN)
install.packages(c("devtools", "pkgdown"), lib = LIB, repos = CRAN)
devtools::install(
  lib = LIB,
  dependencies = TRUE,
  quiet = TRUE,
  build_vignettes = TRUE
)
pkgdown::clean_site()
pkgdown::build_site(lazy = FALSE)

# flag this as a non-Jekyll site for GitHub Pages
file.create("./docs/.nojekyll")
