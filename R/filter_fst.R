#===============================================================================
#' @title Obtain data from rbr sqlite3 database
#'
#' @description Import sqlite data to R
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param db_name the path to the rbr database ( rsk )
#' @param start_date character date
#' @param end_date character date
#' @param well_names character specific wells to subset
#'
#' @return data.table of results
#'
#' @export
#===============================================================================
filter_fst <- function(db_name, start_date, end_date, well_names = NA){

  datetime <- NULL
  name <- NULL

  end <- as.numeric(anytime::anytime(end_date, asUTC = TRUE))
  start <- as.numeric(anytime::anytime(start_date, asUTC = TRUE))

  # get first records
  wl <- fst::read.fst(db_name,
                      as.data.table = TRUE,
                      'datetime',
                      from = 1, to = 2)

  # calculate the time interval
  time_difference <- diff(wl$datetime)
  wl_start <- wl$datetime[1]


  ind_start <- ((start - wl_start) / time_difference)
  ind_end <- ((end - wl_start) / time_difference)


  if (ind_start < 0 & ind_end < 0) {

    print('no records available between start and end date times')
    return()

  } else if (ind_start < 0 & ind_end > 0) {

    ind_start <- 1

  }

  dat <- tryCatch(fst::read.fst(db_name,
                                as.data.table = TRUE,
                                from = ind_start + 1,
                                to = ind_end + 1),
                                error = function(e) e)

  if (inherits(dat, "error")) {
    print('no records available between start and end date times')
    return()
  }

  dat <- dat[, datetime := anytime::anytime(datetime, asUTC = TRUE)]

  if (!is.na(well_names)) {
    dat <- dat[name %in% well_names]
  }

  print('here')
  return(dat)

}
