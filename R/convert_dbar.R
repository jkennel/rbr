#===============================================================================
#' @title convert_dbar
#'
#' @description convert values from dbar
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param val value in dbar to convert
#' @param to_unit abbreviation for unit ('m', 'ft')
#'
#' @return data.table of results
#'
#' @export
#'
#===============================================================================
convert_dbar <- function(val, to_unit = 'm'){
  conv_unit(conv_unit(val, 'dbar', 'cmH2O'), 'cm', to_unit)
}
