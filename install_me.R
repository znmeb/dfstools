#! /usr/bin/env Rscript

library <- Sys.getenv("R_LIBS_USER")
install.packages(c("devtools", "pkgdown"), lib = library)
devtools::install(
  lib = library,
  dependencies = TRUE,
  quiet = TRUE,
  build_vignettes = TRUE
)
pkgdown::clean_site()
pkgdown::build_site(lazy = FALSE)

# flag this as a non-Jekyll site for GitHub Pages
file.create("./docs/.nojekyll")
