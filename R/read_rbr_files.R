#===============================================================================
#' @title obtain data from a list of rbr sqlite3 databases
#'
#' @description import sqlite data to R
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param files the path to the rbr database ( rsk )
#' @param use_rbr_tz boolean use time zone information from the rbr file?
#'
#' @return data.table of results
#'
#' @export
#===============================================================================
read_rbr_files <- function( files, use_rbr_tz = TRUE) {

  # hack for 'global variables' NOTE
  name <- NULL

  dtl <- lapply( files, function(f){
    dt <- read_rbr(f, use_rbr_tz = use_rbr_tz)
    dt[, name := f]
    return(dt)
  } )

  dtl <- dtl[sapply( dtl, nrow ) != 0]

  return( data.table::rbindlist(dtl) )

}
