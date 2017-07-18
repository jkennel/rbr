#===============================================================================
#' @title obtain data from diver .mon file
#'
#' @description import diver data to R
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param db_name character the path to the rbr database ( rsk )
#' @param tz character the timezone of the input data file
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

  wl[, datetime := anytime::anytime(paste(date, time))]

  if (nrow(wl) > 0) {
    date_1 <- anytime::anytime(wl$datetime[1], asUTC = TRUE )
    shift <- difftime(as.POSIXct('1970-01-01', tz = tz),
                      as.POSIXct('1970-01-01', tz = 'UTC'),
                      units = 'secs')
    wl[, datetime := anytime::anytime(datetime + shift, asUTC = TRUE)]
    setkey(wl, datetime)
  }

  wl[, date := NULL]
  wl[, time := NULL]


}
