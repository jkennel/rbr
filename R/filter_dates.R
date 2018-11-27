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
#' @param include_filt_cols include the columns in the filter table
#'
#' @return filtered data.table
#'
#' @export
#===============================================================================
filter_dates <- function(dat, filt, keep = FALSE, which = FALSE,
                         include_filt_cols = FALSE){

  if (!'name' %in% names(filt) |
      !'name' %in% names(dat)) {

    setkey(filt, start, end)
    filt[, id := 1:nrow(filt)]

    if (include_filt_cols) {
      inds <- foverlaps(dat[, list(start=datetime, end=datetime)],
                        filt,
                        type="within", which=TRUE)

      out <- dat[!is.na(inds$yid)][, id := na.omit(inds$yid)]
      setkey(out, id)
      setkey(filt, id)
      return(out[filt[, -c('start', 'end'), with = FALSE], nomatch = 0L])
    }
    inds <- foverlaps(dat[, list(start=datetime, end=datetime)],
                      filt,
                      type="within", which=TRUE)
  } else {

    setkey(filt, name, start, end)
    inds <- foverlaps(dat[, list(name, start=datetime, end=datetime)],
                      filt,
                      type="within", which = TRUE)
  }

  if (which) {

    return(inds$yid)

  }

  if (keep) {

    return(dat[!is.na(inds$yid)])

  } else {

    return(dat[is.na(inds$yid)])

  }

}

# compare_manual <- function(dat, blended, depths) {
#   comp <- filter_dates(dat, blended, keep = TRUE, include_filt_cols = TRUE)
#   comp <- comp[depths]
# }


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

  # hack for 'global variables' NOTE
  name = NULL
  datetime = NULL
  start = NULL
  end = NULL
  val = NULL


  dat[J(shift), on = .(name, datetime >= start, datetime <= end), val := val + shift]

}
