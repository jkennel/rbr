#===============================================================================
#' @title obtain data from a list of rbr sqlite3 databases
#'
#' @description import sqlite data to R
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param files the path to the rbr database ( rsk )
#' @param tz the timezone of the input data file
#'
#' @return data.table of results
#'
#' @export
#===============================================================================
read_rbr_files <- function( files, tz='UTC') {

  # hack for 'global variables NOTE
  name <- NULL

  dtl <- lapply( files, function(f){
    dt <- read_rbr(f, tz=tz)
    dt[, name:=f]
    return(dt)
  } )

  return( data.table::rbindlist(dtl) )

}
