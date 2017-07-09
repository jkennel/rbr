#===============================================================================
#' @title Obtain data from fst file
#'
#' @description Import fst data to R
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param db_name the path to the rbr database ( rsk )
#'
#' @return data.table of results
#'
#' @export
#===============================================================================
read_fst <- function(db_name){

  datetime <- NULL
  name <- NULL
  # get first records
  wl <- fst::read.fst(db_name,
                      as.data.table = TRUE)

  wl <- wl[, datetime := anytime::anytime(datetime, asUTC = TRUE)]

  return(wl)

}
