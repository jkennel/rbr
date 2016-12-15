#===============================================================================
#' @title Obtain data from rbr sqlite3 database
#'
#' @description Import sqlite data to R
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param db_name the path to the rbr database ( rsk )
#' @param start_date character date
#' @param end_date character date
#' @param tz the timezone of the input data file
#'
#' @return data.table of results
#' @import data.table
#' @importFrom RcppCCTZ tzDiff
#' @importFrom dplyr '%>%'
#' @importFrom dplyr collect
#' @importFrom dplyr select
#' @importFrom dplyr sql
#' @importFrom dplyr src_sqlite
#' @importFrom dplyr tbl
#' @export
#===============================================================================
filter_rbr <- function( db_name, start_date, end_date, tz='UTC' ) {

  # convert text to POSIXct
  start_date <- anytime::anytime(start_date, asUTC = TRUE)
  end_date <- anytime::anytime(end_date, asUTC = TRUE)

  sql_text <- paste0("SELECT tstamp/1000.0 as datetime, * FROM data WHERE tstamp >= ",
                     as.numeric( start_date )*1000.0, " AND tstamp <= ",
                     as.numeric( end_date )*1000.0 )

  return( read_rbr_db( db_name, sql_text, tz ) )

}
