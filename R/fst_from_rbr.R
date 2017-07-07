#===============================================================================
#' @title obtain data from rbr sqlite3 database and save in fst format
#'
#' @description convert sqlite data to fst file
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param db_name character the path to the rbr database ( rsk )
#' @param well_name character the well_name to add to the file
#'
#' @return the dataset
#'
#' @export
#===============================================================================
fst_from_rbr <- function(db_name, well_name){

  datetime <- NULL
  channel01 <- NULL

  dat <- read_rbr(db_name)[, list(datetime, val = channel01)]

  db_name <- gsub(' ', '_', db_name)
  db_name <- gsub('.rsk', '', db_name)
  dat[, name := well_name]

  setkey(dat, datetime)

  fst::write.fst(dat,
            paste0('/home/jkennel/Documents/ssfl/data/fst/', db_name, '.fst'))

  invisible(dat)
}
