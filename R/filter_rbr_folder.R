#===============================================================================
#' @title obtain data from folder containing rbr sqlite3 databases
#'
#' @description import sqlite data to R
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param folder_path the path to the rbr database ( rsk )
#' @param start_date character date
#' @param end_date character date
#' @param tz the timezone of the input data file
#' @param ... arguments to pass to list.files
#'
#' @return data.table of results
#'
#' @export
#===============================================================================
filter_rbr_folder <- function( folder_path, start_date, end_date, tz='UTC', ... ) {

  files <- list.files( folder_path, full.names=TRUE, pattern="*.rsk", ... )

  return( filter_rbr_files(files, start_date, end_date, tz=tz) )

}
