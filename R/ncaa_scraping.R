#' @title NCAA Schools
#' @name ncaa_schools
#' @description Gets a table of school names and IDs from the NCAA
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom rvest html_session
#' @importFrom rvest html_node
#' @importFrom rvest html_form
#' @export ncaa_schools
#' @return a tibble with two columns, `school_name` and
#' `school_id`, both character-valued.
#' @details based on <https://github.com/octonion/basketball-w/blob/master/ncaa/scrapers/ncaa_schools.rb>
#' @examples
#' \dontrun{
#' school_list <- dfstools::ncaa_schools()
#' }

ncaa_schools <- function() {
  career_search_url <-
    "http://web1.ncaa.org/stats/StatsSrv/careersearch"
  career_search_session <- rvest::html_session(career_search_url)
  search_form <- career_search_session %>%
    rvest::html_node("form[name=teamSearch]") %>%
    rvest::html_form()
  school_list <-
    search_form[["fields"]][["searchOrg"]][["options"]]
  school_list <- t(t(sapply(school_list, c))) %>%
    tibble::as_tibble(rownames = "school_name")
  colnames(school_list)[2] <- "school_id"
  return(school_list %>% dplyr::filter(school_name != "All"))
}

utils::globalVariables(c(
  "school_name"
))
