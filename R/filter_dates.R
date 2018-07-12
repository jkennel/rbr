
#===============================================================================
#' @title filter_dates
#'
#' @description remove values from a data.table (non-equi join filter)
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param dat data.table to filter (name, datetime)
#' @param filt data.table of filter start and end times (start, end)
#' @param keep include or exclude the subsets
#' @param which return the indices that match
#'
#' @return filtered data.table
#'
#' @export
#===============================================================================
filter_dates <- function(dat, filt, keep = FALSE, which = FALSE){

  setkey(filt, name, start, end)
  inds <- foverlaps(dat[, list(name, start=datetime, end=datetime)],
                    filt,
                    type="within", which=TRUE)

  if (which) {

    return(inds$yid)

  }

  if (keep) {

    return(dat[!is.na(inds$yid)])

  } else {

    return(dat[is.na(inds$yid)])

  }

}



#===============================================================================
#' @title shift_values_range
#'
#' @description shift values for specific regions
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param dat data.table to filter (name, datetime)
#' @param shift data.table of filter start and end times (start, end)
#'
#' @return data.table with shifted values
#'
#' @export
#===============================================================================
shift_values_range <- function(dat, shift){

  dat[J(shift), on = .(name, datetime >= start, datetime <= end), val := val + shift]

}
