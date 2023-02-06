#===============================================================================
#' @title obtain meta data from rbr sqlite3 database
#'
#' @description get supplementary info from .rsk file
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param db_name character the path to the rbr database ( rsk )
#'
#' @return list of results
#'
#' @export
#'
#===============================================================================
rbr_info <- function(db_name) {

  # connect to sqlite database
  db <- DBI::dbConnect(RSQLite::SQLite(), db_name)

  nm_tbl <- DBI::dbListTables(db)

  suppressWarnings({
    channel <-  data.table::melt(
      as.data.table(dplyr::collect(dplyr::tbl(db, 'channels'))),
      id.vars = 'channelID',
      variable.factor = FALSE)
    instrumentChannels <-  data.table::melt(
      as.data.table(dplyr::collect(dplyr::tbl(db, 'instrumentChannels'))),
      id.vars = 'channelID',
      variable.factor = FALSE)
    channel_info <- rbindlist(list(channel, instrumentChannels))


    parameters <-  data.table::melt(
      data.table(dplyr::collect(dplyr::tbl(db, 'parameters'))),
      id.vars = 'deploymentID',
      variable.factor = FALSE)
    deployments <-  data.table::melt(
      data.table(dplyr::collect(dplyr::tbl(db, 'deployments'))),
      id.vars = 'deploymentID',
      variable.factor = FALSE)
    epochs <-  data.table::melt(
      data.table(dplyr::collect(dplyr::tbl(db, 'epochs'))),
      id.vars = 'deploymentID',
      variable.factor = FALSE)
    appSettings <-  data.table::melt(
      data.table(dplyr::collect(dplyr::tbl(db, 'appSettings'))),
      id.vars = 'deploymentID',
      variable.factor = FALSE)

    deployment_info <- rbindlist(list(parameters, deployments, appSettings))

    calibrations <-  melt(dplyr::collect(dplyr::tbl(db, 'calibrations')),
                          id.vars = c('calibrationID', 'channelOrder', 'serialID'),
                          variable.factor = FALSE)

    calibration_info <- calibrations
  })

  list(name = db_name,
       channel_info = channel_info,
       deployment_info = deployment_info,
       calibration_info = calibration_info)


}
