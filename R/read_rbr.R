#===============================================================================
#' @title obtain data from rbr sqlite3 database
#'
#' @description import sqlite data to R
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param db_name the path to the rbr database ( rsk )
#' @param use_rbr_tz boolean use time zone information from the rbr file?
#'
#' @return data.table of results
#'
#' @export
#'
#===============================================================================
read_rbr <- function(db_name, use_rbr_tz = TRUE) {

  sql_text <- "SELECT tstamp/1000.0 as datetime, * FROM data"
  return(read_rbr_db(db_name, sql_text, use_rbr_tz))

}

