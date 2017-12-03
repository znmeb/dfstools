#' @title Install MySportsFeeds Wrapper
#' @name install_mysportsfeeds_wrapper
#' @description Installs the MySportsFeed R wrapper from GitHub
#' @export install_mysportsfeeds_wrapper
#' @return a vector of points
#' @examples
#' \dontrun{
#' tidysportsfeeds::install_mysportsfeeds_wrapper()
#' }

install_mysportsfeeds_wrapper <- function() {
  devtools::install_github("MySportsFeeds/mysportsfeeds-r")
}
