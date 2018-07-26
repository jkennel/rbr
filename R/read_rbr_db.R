#===============================================================================
#' @title obtain data from rbr sqlite3 database
#'
#' @description import sqlite data to R
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param db_name character the path to the rbr database ( rsk )
#' @param sql_text character sql string to execute on connection for filtering
#' @param use_rbr_tz boolean use time zone information from the rbr file?
#'
#' @return data.table of results
#'
#' @export
#'
#===============================================================================
read_rbr_db <- function(db_name, sql_text, use_rbr_tz = TRUE) {

  # .rsk stores date as numeric and is referenced to UTC
  # hack for 'global variables NOTE
  tstamp <- NULL
  datetime <- NULL
  datasetID <- NULL

  # connect to sqlite database
  db <- dplyr::src_sqlite(db_name)
  nm_tbl <- dplyr::src_tbls(db)

  # get column names
  if (!any(grepl('channels', nm_tbl))) {
    warning(paste(db_name, 'does not have a table called "channels".  Check .rsk file.'))
    return(NULL)
  } else {
    channels <- collect(tbl(db, 'channels'))$shortName
  }

  # get the tz information
  if (use_rbr_tz) {
    if (any(grepl('parameterKeys', nm_tbl))) {
      tz_offset <- data.table(collect(tbl(db, 'parameterKeys')))
      tz_offset <- tz_offset[key == 'OFFSET_FROM_UTC']$value
      print(tz_offset)
      tz_offset <- as.numeric(tz_offset) * 3600
      if(is.na(tz_offset)) {
        warning(paste0('Time zone offset from UTC is NA, using 0'))
        tz_offset <- 0
      }
    } else if (any(grepl('parameters', nm_tbl))) {
      tz_offset <- data.table(collect(tbl(db, 'parameters')))
      tz_offset <- tz_offset$offsetfromutc
      tz_offset <- as.numeric(tz_offset) * 3600
      if(is.na(tz_offset)) {
        warning(paste0('Time zone offset from UTC is NA, using 0'))
        tz_offset <- 0
      }
    }
  } else {
      warning(paste0('Time zone offset from UTC is missing, using 0'))
      tz_offset <- 0
  }

  # check if any data is present
  if (!any(grepl('data', nm_tbl))) {
    warning(paste(db_name, 'does not have a table called "data".  Check .rsk file.'))
    return(NULL)
  } else {

    # time is in milliseconds
    dt <- dplyr::tbl(db, dplyr::sql(sql_text))   %>%
      select(-tstamp)

    # remove the datasetID column if it exists
    fields <- dplyr::tbl_vars(dt)
    if ("datasetID" %in% fields) dt <- dt %>% select(-datasetID)

    # read data into data.table and set key
    dt <- data.table::setDT(collect(dt, n = Inf), key = datetime)

    # make sure it has the correct timezone
    # only single shift is allowed for all times
    if (nrow(dt) > 0) {
      if(tz_offset != 0) {
        tz_text <- paste0('UTC', tz_offset / 3600, 'h')
      } else {
        tz_text <- 'UTC'
      }
      date_1 <- anytime::anytime(dt$datetime[1], asUTC = TRUE)
      dt[, datetime := anytime::anytime(datetime + tz_offset, asUTC = TRUE)]
      setnames(dt, c('datetime', channels))
      setkey(dt, datetime)
      attr(dt$datetime, 'tzone') <- tz_text
      return(melt(dt, id.vars = 'datetime'))
    } else {
      return(NULL)
    }
  }
}

