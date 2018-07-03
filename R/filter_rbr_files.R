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
#' @param use_rbr_tz boolean use time zone information from the rbr file?
#'
#' @return data.table of results
#'
#' @export
#===============================================================================
filter_rbr_files <- function(files, start_date, end_date, use_rbr_tz = TRUE) {

  # hack for 'global variables NOTE
  name <- NULL

  dtl <- lapply( files, function(f){
    dt <- filter_rbr(f, start_date, end_date, use_rbr_tz = use_rbr_tz)
    if(!is.null(dt)) {
      return(dt[, name := f])
    } else {
      return(NULL)
    }
  } )

  # remove null rbr files
  dtl <- dtl[!is.null(dtl)]

  return( data.table::rbindlist(dtl) )

}

