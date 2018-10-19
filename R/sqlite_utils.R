#' @title Connect Database File
#' @name connect_database_file
#' @description creates an SQLite database file if needed and returns its connection
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect
#' @export connect_database_file
#' @param path full path to file. It will be created if it doesn't exist
#' @return a DBI connection object
#' @examples
#' \dontrun{
#' library(dfstools)
#' connection <- connect_database_file("~/DFS/nba/dfs_database.sqlite")
#' }

connect_database_file <- function(path) {
  return(DBI::dbConnect(drv = RSQLite::SQLite(), dbname = path))
}

#' @title Append Table
#' @name append_table
#' @description creates a table if needed and appends a data frame to it
#' @importFrom DBI dbExistsTable
#' @importFrom DBI dbCreateTable
#' @importFrom DBI dbAppendTable
#' @export append_table
#' @param connection connection to the database
#' @param table the table name. It will be created if it doesn't exist
#' @param dataframe the data frame to append
#' @return a list of two items
#' \itemize{
#' \item table_create logical was the table created?
#' \item table_append integer the number of rows appended to the table
#' }

append_table <- function(connection, table, dataframe) {
  table_create <- FALSE
  if (!DBI::dbExistsTable(connection, table)) {
    table_create <- DBI::dbCreateTable(connection, table, dataframe)
  }
  table_append <- DBI::dbAppendTable(connection, table, dataframe)
  return(list(table_create = table_create, table_append = table_append))
}
