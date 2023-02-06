#===============================================================================
#' @title rbr_table_names
#'
#' @description get the names of the tables in the .rsk file
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param db_name the path to the rbr database ( rsk )
#'
#' @return vector of table names
#'
#' @export
#'
#===============================================================================
rbr_table_names <- function(db_name) {

  db <- DBI::dbConnect(RSQLite::SQLite(), db_name)

  return(dplyr::src_tbls(db))

}


#===============================================================================
#' @title rbr_tables
#'
#' @description get the raw tables from the .rsk file
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param db_name the path to the rbr database ( rsk )
#' @param which_tables the names of the specific tables to get, NA get all tables
#'
#' @return the .rsk file as a list of tables
#'
#' @export
#'
#===============================================================================
rbr_tables <- function(db_name, which_tables = NA) {


  db <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  tn <- DBI::dbListTables(db)

  # subset of tables to get
  if(!all(is.na(which_tables))) {
    tn <- intersect(which_tables, tn)
  }

  if(length(tn) == 0) {
    stop('no tables in .rsk with names matching *which_tables*')
  }

  dat <- lapply(tn, function(x) dplyr::collect(dplyr::tbl(db, x)))
  dat <- lapply(dat, int64_to_posix)
  names(dat) <- tn

  return(dat)

}


#===============================================================================
#' @title int64_to_posix
#'
#' @description convert integer64 to posix
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param dt the table of data
#'
#' @return data.table with integer64 converted to POSIXct
#'
#' @export
#'
#===============================================================================
int64_to_posix <- function(dt) {

  setDT(lapply(dt, function(x) {
    if(inherits(x, 'integer64')) {
      anytime::anytime(x / 1000, asUTC = TRUE)
    } else {
      x
    }
  }))

}
