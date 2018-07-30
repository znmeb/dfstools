dfstools - Tidy Data Analytics from Sports Data APIs
================

[![Build
Status](https://travis-ci.org/znmeb/dfstools.svg?branch=master)](https://travis-ci.org/znmeb/dfstools)

## Getting started

1.  Read the [code of
    conduct](https://github.com/znmeb/dfstools/blob/master/CONDUCT.md)
    and [contributor’s
    guide](https://github.com/znmeb/dfstools/blob/master/CONTRIBUTING.md).
2.  Sign up for an account at
    [MySportsFeeds.com](https://www.mysportsfeeds.com/) and
    [Stattleship.com](https://api.stattleship.com/). You will need
    accounts on both to use this package; the two APIs have different
    capabilities and the R package uses both APIs.
3.  Sign up for monthly support of the API providers on Patreon. After
    you do that, they will activate your logins / API tokens.
      - [MySportsFeeds.com](https://www.patreon.com/mysportsfeeds)
      - [Stattleship.com](https://www.patreon.com/stattleship)
4.  If you find this package useful, you can support my work on Patreon
    at <https://www.patreon.com/znmeb/posts>. ***Supporting me is not
    required to use the package\!***

## Installing the package

1.  Install git, R and RStudio for your work environment. This is a
    developers’ release; for the moment, you’ll need to be familiar with
    git / GitHub, R and RStudio.
    
    I test regularly on Windows and Arch Linux, but any environment that
    supports RStudio should work. If anything doesn’t work or the
    documentation is unclear, please file an issue at
    <https://github.com/znmeb/dfstools/issues/new>

2.  Open RStudio and install the packages `devtools` and `roxygen2` from
    CRAN.

3.  In the RStudio console, type
    `devtools::install_github("znmeb/dfstools")`.

## Some philosophy / roadmap

I’m planning to grow this package some, but not as a full-fledged DFS
analytics suite. Most of it will be on an as-needed basis; I’ll want to
run an analysis and I’ll code up the API interfaces I need for the task.

The four tasks I have coded (subject to some refactoring to be done) are

1.  Predicting game scores and win probabilities via `mvglmmRank`,
2.  Visualization of players’ and teams’ fantasy point distributions
    with `ggplot2`,
3.  Archetypoidal analysis (`Anthropometry`), and
4.  Predicting fantasy points via a general linear model (`MASS::glm`).

## Patreon links

  - [M. Edward (Ed) Borasky](https://www.patreon.com/znmeb/posts)
  - [MySportsFeeds](https://www.patreon.com/mysportsfeeds/posts)
  - [Stattleship](https://www.patreon.com/stattleship/posts)
