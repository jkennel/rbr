#===============================================================================
#' @title obtain data from rbr sqlite3 database
#'
#' @description import sqlite data to R
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param db_name character the path to the rbr database ( rsk )
#' @param sql_text character sql string to execute on connection for filtering
#' @param tz character the timezone of the input data file
#'
#' @return data.table of results
#'
#' @export
#===============================================================================
read_rbr_db <- function( db_name, sql_text, tz='UTC' ) {

  # hack for 'global variables NOTE
  tstamp <- NULL
  datetime <- NULL
  datasetID <- NULL

  # connect to sqlite database
  db <- dplyr::src_sqlite( db_name )
  #src_tbls( db )

  # get transducer and unit info
  unit <-  dplyr::collect( tbl( db, sql( "SELECT units FROM channels" ) ) )[[1]]
  id   <-  dplyr::collect( tbl( db, sql( "SELECT serialID FROM instruments" ) ) )[[1]]

  # time is in milliseconds
  dt <- dplyr::tbl( db, dplyr::sql(sql_text) )   %>%
    select( -tstamp )

  # remove the datasetID column if it exists
  fields <- dplyr::tbl_vars(dt)
  if("datasetID" %in% fields) dt %>% select(-datasetID)

  dt <- data.table::setDT( collect( dt, n=Inf )  )

  # make sure it has the correct timezone
  dt[, datetime:= as.POSIXct( datetime,  origin = "1970-01-01", tz=tz, usetz=TRUE )]

  # set the timezone
  if(tz != 'UTC'){
    attributes(dt$datetime)$tzone <- "UTC"
  }

  data.table::setkey( dt, datetime )

  return( dt )

}

