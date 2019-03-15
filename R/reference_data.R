#' #' locate_transducer
#' #'
#' #' Sometimes the depth of the transducer is unknown due to incomplete deployment,
#' #' knots or kinks in the transducer string/wire.  This method compares the transducer
#' #' pressure to a manual water level measurement
#' #'
#' #' @param rbr table table with datetime, pressure, and barometric pressure
#' #' @param man table with datetime and water level elevation
#' #' @param datetime_name name of the datetime column rbr and man tables
#' #' @param pres_name name of the pressure column rbr table
#' #' @param baro_name name of the baro column in rbr table
#' #' @param wl_elev_name name of the water level elevation in man table
#' #'
#' #' @return pressure to elevation
#' #'
#' #' @export
#' #'
#' locate_transducer <- function(rbr,
#'                               man,
#'                               datetime_name,
#'                               pres_name,
#'                               baro_name,
#'                               wl_elev_name,
#'                               depth_unit = 'm',
#'                               reference = '0') {
#'
#'   dat <- copy(rbr)
#'   setnames(dat, c(datetime_name, pres_name, baro_name),
#'                 c('datetime', 'val', 'ba'))
#'
#'   wl  <- copy(man)
#'   setnames(wl, c(datetime_name, wl_elev_name),
#'                c('datetime', 'wl'))
#'
#'
#'   dat[, ba_rem := (val - ba)]
#'
#'   # height of water above transducer
#'   dat[, ba_rem := ud.convert(ud.convert(ba_rem, 'dbar', 'cmH2O'), 'cm', depth_unit)]
#'
#'
#'
#'
#'
#' }
#'
#' library(rbr)
#' rbr <- read_rbr_files('/media/kennel/Data/tmp/rbr_19_03_07/RD05C_078061_20190307_1538.rsk')
#'
#' #' reference_data
#' #'
#' #' Convert pressure measurements to elevation, depth to water and ensure
#' #' consistent units
#' #'
#' #' @param data_depth table with id, path, and depth of the transducers
#' #' @param id_col column with the transducer id
#' #' @param path_col column with the path to the .rsk file
#' #' @param depth_col depth of the transducer from reference
#' #' @param type_col type of data being collected
#' #' @param elev_unit reference elevation units
#' #' @param ref_elev elevation of the reference
#' #' @param start_date starting date for the reference
#' #' @param end_date end date for the reference
#' #'
#' #' @return pressure to elevation
#' #'
#' #' @export
#' #'
#' reference_data <- function(data_depth,
#'                            id_col = 'id',
#'                            path_col = 'path',
#'                            depth_col = 'depth',
#'                            type_col = 'baro',
#'                            elev_unit = 'm',
#'                            ref_elev = 0,
#'                            start_date,
#'                            end_date
#'
#' ) {
#'
#'   dat <- copy(data_depth)
#'
#'   setnames(dat, type_col, 'baro')
#'
#'   if (sum(dat$baro, na.rm = TRUE) == 0) {
#'     stop('Must have barometric data for calculations')
#'   }
#'
#'   # Get baro data
#'   baro <- filter_rbr_files(dat[file.exists(path) & baro]$path,
#'                            start_date = start_date,
#'                            end_date = end_date,
#'                            use_rbr_tz = FALSE)
#'
#'   baro <- baro[grepl('pres', variable)]
#'
#'
#'   baro <- baro[, list(baro = mean(value)), by = datetime]
#'
#'   # Get pressure data
#'   port <- filter_rbr_files(dat[file.exists(path) & !baro]$path,
#'                            start_date = start_date,
#'                            end_date = end_date,
#'                            use_rbr_tz = FALSE)
#'   port <- port[grepl('pres', variable)]
#'
#'   # Unit conversion
#'   baro[, baro := measurements::conv_unit(measurements::conv_unit(baro, 'dbar', 'cmH2O'), 'cm', elev_unit)]
#'   port[, value := measurements::conv_unit(measurements::conv_unit(value, 'dbar', 'cmH2O'), 'cm', elev_unit)]
#'
#'   # Join baro and port data
#'   port <- port[baro[, list(datetime, baro)], on = 'datetime', nomatch = 0L]
#'
#'   # Convert to equivalent water height
#'   setnames(dat, c(id_col, path_col, depth_col) , c('id', 'name', 'depth'))
#'   port[, variable := NULL]
#'   port[dat[, list(name, depth)], `:=`(dtw = (i.depth - (value - baro)),
#'                                       depth = depth), on = 'name']
#'   port[dat[, list(name, id)], name := (i.id), on = 'name']
#'   port[, elev := ref_elev - dtw - baro, on = 'name']
#'   port[, list(name, depth, datetime, value, baro, dtw, elev)]
#'
#' }
#'
#'
#' # library(plotly)
#' # library(data.table)
#' # library(rbr)
#' # data_depth <- fread('/media/kennel/Data/phd/personnel/pat/transducer_depth_path.csv')
#' #
#' # port <- reference_data(data_depth,
#' #                        id_col = 'id', path_col = 'path', depth_col = 'depth', elev_unit = 'm',
#' #                        start = '2014-12-08 00:00:00', end = '2014-12-09 00:00:00',
#' #                        ref_elev = 309.33 + 0.319)
#' #
#' # blended <- fread('/media/kennel/Data/phd/personnel/pat/blended_open_hole.csv')
#' # blended <- blended[qualifier == 'wl']
#' #
#' # dates <- seq.POSIXt(as.POSIXct('2014-12-08 14:30', tz = 'UTC'),
#' #                     as.POSIXct('2014-12-08 15:20', tz = 'UTC'), '1 min')
#' # blended <- data.table(start = dates[-length(dates)],
#' #                       end = dates[-1],
#' #                       meas_wl = 17.45)
#' # blended <- blended[seq(1, nrow(blended), 2)]
#' # #plot_ly(dat[name == 'P1_01'], x = ~datetime, y = ~value, type= 'scatter', mode = 'lines')
#' #
#' # dat <- filter_dates(port, blended, keep = TRUE, include_filt_cols = TRUE)
#' # tmp <- dat[, list(mean_diff = mean(dtw-meas_wl, na.rm = TRUE)),
#' #            by = list(name, id, depth)]
#' #
#' # tmp[, list(adjust = median(mean_diff)), by = list(name)]
#' # library(plotly)
#' # library(viridis)
#' # length(unique(tmp$id))
#' # plot_ly(tmp, x = ~depth, y = ~mean_diff,
#' #         name = ~name,
#' #         color = ~id,
#' #         colors = viridis(length(unique(tmp$id))),
#' #         type='scatter', mode = 'markers')
#'
#'
