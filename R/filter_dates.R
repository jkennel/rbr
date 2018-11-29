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
#' @param include_filt_cols include the columns in the filter table when keep is TRUE
#'
#' @return filtered data.table
#'
#' @export
#===============================================================================
filter_dates <- function(dat, filt,
                         keep = FALSE,
                         include_filt_cols = FALSE) {


  # filter with or without name column
  if (!'name' %in% names(filt) |
      !'name' %in% names(dat)) {

    setkey(filt, start, end)
    key_nm <- key(filt)
    dat_key <- dat[, list(start = datetime, end = datetime)]
    rem_col <- NULL

  } else {

    setkey(filt, name, start, end)
    key_nm <- key(filt)
    dat_key <- dat[, list(name, start = datetime, end = datetime)]

    rem_col <- 'name'

  }

  setkeyv(dat_key, key_nm)


  # match intervals
  inds <- na.omit(foverlaps(dat_key,
                            filt,
                            which = TRUE,
                            type="within"), "yid")

  # return the data.table inside the filter ranges
  if (keep) {

    # return the data.table inside the filter ranges with filter data
    if (include_filt_cols) {


      # which filter group
      filt[, id := 1:nrow(filt)]

      out <- dat[inds$xid][, id := inds$yid]

      setkey(out, id)
      setkey(filt, id)

      return(out[filt[, -c(rem_col), with = FALSE], nomatch = 0L][, -c('id'), with = FALSE])

    } else {

      return(dat[unique(inds$xid)])

    }


  # return the data.table outside of the filter ranges
  } else {

    return(dat[!unique(inds$xid)])

  }

}

# compare_manual <- function(dat, blended, depths) {
#   comp <- filter_dates(dat, blended, keep = TRUE, include_filt_cols = TRUE)
#   comp <- comp[depths]
# }




# library(data.table)
# library(rbr)
#
# wl <- data.table(datetime = seq(as.POSIXct('2012-01-01'), as.POSIXct('2012-05-01'), 1))
# wl[, val := rnorm(nrow(wl))]
# wl[, name := 'well_1']
#
# shift <- data.table(start = c(as.POSIXct('2012-02-01'),
#                               as.POSIXct('2012-02-15')),
#                     end = c(as.POSIXct('2012-03-01'),
#                             as.POSIXct('2012-03-15')),
#                     adj = c(1, 2))
#
# #shift[, name := c('well_1', 'well_1')]
#
# system.time(
#
# a <- filter_dates(wl, shift,
#                   keep = TRUE,
#                   include_filt_cols = TRUE)
# )
#
# system.time(
#
#   a <- filter_dates(wl, shift,
#                     keep = TRUE,
#                     include_filt_cols = FALSE)
# )
#
# system.time(
#
#   a <- filter_dates(wl, shift,
#                     keep = FALSE,
#                     include_filt_cols = TRUE)
# )
