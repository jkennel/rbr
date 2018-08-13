#' reference_data
#'
#' Convert pressure measurements to elevation, depth to water and ensure
#' consistent units
#'
#' @param data_depth table with id, path, and depth of the transducers
#' @param id_col column with the transducer id
#' @param path_col column with the path to the .rsk file
#' @param depth_col depth of the transducer from reference
#' @param type_col type of data being collected
#' @param elev_unit reference elevation units
#' @param ref_elev elevation of the reference
#' @param start_date starting date for the reference
#' @param end_date end date for the reference
#'
#' @return
#' @export
#'
reference_data <- function(data_depth,
                           id_col = 'id',
                           path_col = 'path',
                           depth_col = 'depth',
                           type_col = 'baro',
                           elev_unit = 'm',
                           ref_elev = 0,
                           start_date,
                           end_date

) {

  dat <- copy(data_depth)

  setnames(dat, type_col, 'baro')

  if (sum(dat$baro, na.rm = TRUE) == 0) {
    stop('Must have barometric data for calculations')
  }

  # Get baro data
  baro <- filter_rbr_files(dat[file.exists(path) & baro]$path,
                           start = start_date, end = end_date,
                           use_rbr_tz = FALSE)
  baro <- baro[grepl('pres', variable)]


  baro <- baro[, list(baro = mean(value)), by = datetime]

  # Get pressure data
  port <- filter_rbr_files(dat[file.exists(path) & !baro]$path,
                           start = start_date, end = end_date,
                           use_rbr_tz = FALSE)
  port <- port[grepl('pres', variable)]

  # Unit conversion
  baro[, baro := measurements::conv_unit(measurements::conv_unit(baro, 'dbar', 'cmH2O'), 'cm', elev_unit)]
  port[, value := measurements::conv_unit(measurements::conv_unit(value, 'dbar', 'cmH2O'), 'cm', elev_unit)]

  # Join baro and port data
  port <- port[baro[, list(datetime, baro)], on = 'datetime', nomatch = 0L]

  # Convert to equivalent water height
  setnames(dat, c(id_col, path_col, depth_col) , c('id', 'name', 'depth'))
  port[, variable := NULL]
  port[dat[, list(name, depth)], `:=`(dtw = (i.depth - (value - baro)),
                                      depth = depth), on = 'name']
  port[dat[, list(name, id)], name := (i.id), on = 'name']
  port[, elev := ref_elev - dtw - baro, on = 'name']
  port[, list(name, depth, datetime, value, baro, dtw, elev)]

}


# library(plotly)
# library(data.table)
# library(rbr)
# data_depth <- fread('/media/kennel/Data/phd/personnel/pat/transducer_depth_path.csv')
#
# port <- reference_data(data_depth,
#                        id_col = 'id', path_col = 'path', depth_col = 'depth', elev_unit = 'm',
#                        start = '2014-12-08 00:00:00', end = '2014-12-09 00:00:00',
#                        ref_elev = 309.33 + 0.319)
#
# blended <- fread('/media/kennel/Data/phd/personnel/pat/blended_open_hole.csv')
# blended <- blended[qualifier == 'wl']
#
# dates <- seq.POSIXt(as.POSIXct('2014-12-08 14:30', tz = 'UTC'),
#                     as.POSIXct('2014-12-08 15:20', tz = 'UTC'), '1 min')
# blended <- data.table(start = dates[-length(dates)],
#                       end = dates[-1],
#                       meas_wl = 17.45)
# blended <- blended[seq(1, nrow(blended), 2)]
# #plot_ly(dat[name == 'P1_01'], x = ~datetime, y = ~value, type= 'scatter', mode = 'lines')
#
# dat <- filter_dates(port, blended, keep = TRUE, include_filt_cols = TRUE)
# tmp <- dat[, list(mean_diff = mean(dtw-meas_wl, na.rm = TRUE)),
#            by = list(name, id, depth)]
#
# tmp[, list(adjust = median(mean_diff)), by = list(name)]
# library(plotly)
# library(viridis)
# length(unique(tmp$id))
# plot_ly(tmp, x = ~depth, y = ~mean_diff,
#         name = ~name,
#         color = ~id,
#         colors = viridis(length(unique(tmp$id))),
#         type='scatter', mode = 'markers')


