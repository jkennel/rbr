#===============================================================================
#' @title obtain data from folder containing rbr sqlite3 databases
#'
#' @description import sqlite data to R
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param folder_path the path to the rbr database ( rsk )
#' @param use_rbr_tz boolean use time zone information from the rbr file?
#' @param ... arguments to pass to list.files
#'
#' @return data.table of results
#'
#' @export
#===============================================================================
read_rbr_folder <- function( folder_path, use_rbr_tz = TRUE, ... ) {

  # get the names .rsk files in folder
  files <- list.files( folder_path, full.names = TRUE, pattern = "*.rsk", ... )

  return( read_rbr_files(files, use_rbr_tz = use_rbr_tz))

}

