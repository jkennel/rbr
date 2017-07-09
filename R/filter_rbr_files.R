#===============================================================================
#' @title obtain data from folder containing rbr sqlite3 databases
#'
#' @description import sqlite data to R
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param files the path to the rbr database ( rsk )
#' @param start_date character date
#' @param end_date character date
#' @param tz the timezone of the input data file
#'
#' @return data.table of results
#'
#' @export
#===============================================================================
filter_rbr_files <- function(files, start_date, end_date, tz = 'UTC') {

  # hack for 'global variables NOTE
  name <- NULL

  dtl <- lapply( files, function(f){
    dt <- filter_rbr(f, start_date, end_date, tz = tz)
    dt[, name := f]
    return(dt)
  } )

  dtl <- dtl[sapply( dtl, nrow ) != 0]

  return( data.table::rbindlist(dtl) )

}

