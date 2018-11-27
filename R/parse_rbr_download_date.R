#' parse_rbr_download_date
#'
#' RBR files typically save the date in the downloaded file name.  This function
#' parses that date to a POSIXct
#'
#' @param fn {character} scalar or vector of file name(s)
#' @param tz {character} timezone
#'
#' @return POSIXct datetime
#' @export
#'
#' @examples
#' dt <- parse_rbr_download_date('20141117_1240')
parse_rbr_download_date <- function(fn, tz = 'UTC') {

  ind <- regexpr('\\d{8}\\_\\d{4}', fn)
  tm <- regmatches(fn, ind)
  tm <- as.POSIXct(tm,
                   format = '%Y%m%d_%H%M',
                   tz = tz)

}
