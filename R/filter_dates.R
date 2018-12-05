#===============================================================================
#' @title filter_dates
#'
#' @description remove values from a data.table (non-equi join filter)
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param all data.table to filter (name, datetime)
#' @param subsets data.table of filter start and end times (start, end)
#' @param keep include or exclude the subsets
#' @param include_filt_cols include the columns in the filter table when keep is TRUE
#'
#' @return filtered data.table
#'
#' @export
#===============================================================================
filter_dates <- function(all, subsets,
                         keep = FALSE,
                         include_filt_cols = FALSE) {

  dat <- copy(all)
  filt <- copy(subsets)

  # filter with or without name column
  if (!'name' %in% names(filt) |
      !'name' %in% names(dat)) {

    setkeyv(filt, "start", "end")
    key_nm <- key(filt)
    dat_key <- dat[, list(start = datetime, end = datetime)]
    rem_col <- NULL

  } else {

    setkeyv(filt, "name", "start", "end")
    key_nm <- key(filt)
    dat_key <- dat[, list(name, start = datetime, end = datetime)]

    rem_col <- "name"

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

      setkeyv(out, 'id')
      setkeyv(filt, 'id')

      #out <- out[filt[, -c(rem_col), with = FALSE], nomatch = 0L]
      out <- out[filt, nomatch = 0L]
      #out <- out[, -c('id'), with = FALSE]
      return(out)

    } else {

      return(dat[unique(inds$xid)])

    }


  # return the data.table outside of the filter ranges
  } else {

    return(dat[!unique(inds$xid)])

  }

}



#===============================================================================
#' @title find_nearest
#'
#' @description find the closest value in time
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param transducer data.table of transducer measurements (name, datetime)
#' @param manual data.table manual measurements (name, datetime)
#' @param roll_size numeric maximum time difference (sec) to allow
#'
#' @return transducer measurements within roll_size time
#'
#' @export
#===============================================================================
find_nearest <- function(transducer, manual, roll_size = 86400*7) {


  man <- copy(manual)
  man[, datetime_man := datetime]

  setkey(transducer, name, datetime)
  setkey(man, name, datetime)

  # should be a more efficient way to do this but could not get roll_ends to work
  return(unique(rbind(man[transducer,
                          roll = -100L,
                          nomatch = 0L],
                      man[transducer,
                          roll = 100L,
                          nomatch = 0L])))

}

# library(data.table)
# library(rbr)
# wl <- data.table(datetime = seq(as.POSIXct('2012-01-01'), as.POSIXct('2012-05-01'), 1))
# wl[, val := rnorm(nrow(wl))]
# wl[, name := 'well_1']
#
# shift <- data.table(datetime = as.POSIXct(c('2012-02-15', '2012-02-10', '2012-02-05', '2012-03-01')), adj = c(1, 2, 3, 4))
# shift[, name := 'well_1']
#
# tmp <- unique(find_nearest(wl, shift, roll_size = 100))
# table(tmp$adj)


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
# shift <- data.table(start = seq(as.POSIXct('2012-02-01'),
#                               as.POSIXct('2012-02-15'), 86400),
#                     end = seq(as.POSIXct('2012-02-03'),
#                             as.POSIXct('2012-03-16'), 86400))
# shift[, adj := rnorm(nrow(shift))]
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


