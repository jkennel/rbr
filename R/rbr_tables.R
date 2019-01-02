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

  db <- dplyr::src_sqlite(db_name)

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
#'
#' @return the .rsk file as a list of tables
#'
#' @export
#'
#===============================================================================
rbr_tables <- function(db_name) {

  db <- dplyr::src_sqlite(db_name)
  tn <- dplyr::src_tbls(db)

  return(lapply(tn, function(x) dplyr::collect(dplyr::tbl(db, x))))

}

