#===============================================================================
#' @title obtain data from diver .mon file
#'
#' @description import diver data to R
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param db_name character the path to the rbr database ( rsk )
#' @param tz character the time zone information
#'
#' @return data.table of results
#'
#' @export
#===============================================================================
read_diver <- function(db_name, tz = 'UTC') {

  name <- strsplit(readLines(db_name, 16)[16], '=')[[1]][2]
  n <- as.numeric(readLines(db_name, 53)[53])

  wl <- fread(db_name, skip = 53, header = FALSE, nrows = n)
  setnames(wl, c('date', 'time', 'wl', 'temperature'))

  wl[, name := name]
  wl[, wl := as.numeric(wl)]
  wl[, temperature := as.numeric(temperature)]

  wl[, datetime := anytime::anytime(paste(date, time), tz = 'UTC', asUTC = TRUE)]

  if (nrow(wl) > 0) {
    shift <- difftime(as.POSIXct('1970-01-01', tz = tz),
                      as.POSIXct('1970-01-01', tz = 'UTC'),
                      units = 'secs')
    wl[, datetime := anytime::anytime(datetime + shift, tz = 'UTC', asUTC = TRUE)]
    setkey(wl, datetime)

  }

  wl[, date := NULL]
  wl[, time := NULL]
}
