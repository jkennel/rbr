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
  db <- dplyr::src_sqlite( db_name )

  #nm_tbl <- dplyr::src_tbls(db)

  suppressWarnings({
  channel <-  melt(dplyr::collect(dplyr::tbl(db, 'channels')),
                   id.vars = 'channelID', variable.factor = FALSE)
  instrumentChannels <-  melt(dplyr::collect(dplyr::tbl(db, 'instrumentChannels')),
                              id.vars = 'channelID', variable.factor = FALSE)
  channel_info <- rbindlist(list(channel, instrumentChannels))


  parameters <-  melt(data.table(dplyr::collect(dplyr::tbl(db, 'parameters'))),
                      id.vars = 'deploymentID', variable.factor = FALSE)
  deployments <-  melt(data.table(dplyr::collect(dplyr::tbl(db, 'deployments'))),
                       id.vars = 'deploymentID', variable.factor = FALSE)
  epochs <-  melt(data.table(dplyr::collect(dplyr::tbl(db, 'epochs'))),
                  id.vars = 'deploymentID', variable.factor = FALSE)
  appSettings <-  melt(data.table(dplyr::collect(dplyr::tbl(db, 'appSettings'))),
                       id.vars = 'deploymentID', variable.factor = FALSE)

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
