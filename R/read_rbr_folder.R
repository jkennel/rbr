#===============================================================================
#' @title obtain data from folder containing rbr sqlite3 databases
#'
#' @description import sqlite data to R
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param folder_path the path to the rbr database ( rsk )
#' @param tz the timezone of the input data file
#' @param ... arguments to pass to list.files
#'
#' @return data.table of results
#'
#' @export
#===============================================================================
read_rbr_folder <- function( folder_path, tz='UTC', ... ) {

  files <- list.files( folder_path, full.names=TRUE, pattern="*.rsk", ... )

  return( read_rbr_files(files, tz=tz))

}

