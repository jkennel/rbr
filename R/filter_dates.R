
#===============================================================================
#' @title filter_dates
#'
#' @description remove values from a data.table (non-equi join filter)
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param dat data.table to filter (name, datetime)
#' @param filt data.table of filter start and end times (start, end)
#'
#' @return filtered data.table
#'
#' @export
#===============================================================================
filter_dates <- function(dat, filt){

  dat[!J(filt), on = .(name, datetime >= start, datetime <= end)]

}
