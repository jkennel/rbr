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
read_rbr_db <- function( db_name, sql_text, tz='UTC' ) {

  # connect to sqlite database
  db <- src_sqlite( db_name )
  #src_tbls( db )

  # get transducer and unit info
  unit <-  collect( tbl( db, sql( "SELECT units FROM channels" ) ) )[[1]]
  id   <-  collect( tbl( db, sql( "SELECT serialID FROM instruments" ) ) )[[1]]

  # time is in milliseconds
  dt <- tbl( db, sql(sql_text) ) %>%
          select( -tstamp )

  dt <- setDT( collect( dt, n=Inf )  )

  # make sure it has the correct timezone
  dt[, datetime:= as.POSIXct( datetime,  origin = "1970-01-01", tz=tz, usetz=TRUE )]

  # set the timezone
  if(tz != 'UTC'){
    attributes(dt$datetime)$tzone <- "UTC"
  }

  setkey( dt, datetime )

  return( dt )

}

