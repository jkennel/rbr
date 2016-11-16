#===============================================================================
#' @title obtain data from rbr sqlite3 database
#'
#' @description import sqlite data to R
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param db_name the path to the rbr database ( rsk )
#' @param tz the timezone of the input data file
#'
#' @return data.table of results
#'
#' @export
#===============================================================================
read_rbr <- function( db_name, tz='UTC' ) {

  sql_text <- "SELECT tstamp/1000.0 as datetime, * FROM data"

  return( read_rbr_db( db_name, sql_text, tz ) )

}

