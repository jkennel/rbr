
#===============================================================================
#' @title parse_start
#'
#' @description parse the starting info from an rbr log file
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param search_string text identifier
#' @param x text
#'
#' @return starting info
#'
#' @export
#===============================================================================
parse_start <- function(search_string, x) {

  ind   <- grep(search_string, x, ignore.case = TRUE, useBytes = TRUE)
  m     <- regexpr(': ', x[ind])
  start <- as.numeric(m + 2)
  end   <- stringi::stri_length(x[ind])

  ret   <- stringi::stri_sub(x[ind], start, -1L)
  if (length(ret) == 0) {
    ret <- NA
  }

  ret
}

#===============================================================================
#' @title parse_log_start
#'
#' @description Get transducer info from rbr log file
#'
#' @author Jonathan Kennel \email{jkennel@uoguelph.ca}
#'
#' @param file input file
#'
#' @return starting info
#'
#' @export
#===============================================================================
parse_log <- function(file){
  log_dat <- readLines(file)

  wh <- grep('\\^', log_dat, useBytes = TRUE)
  sub <- split(log_dat, findInterval(1:length(log_dat), wh))

  st <- lapply(sub, function(x) {
    x <- stringi::stri_enc_toutf8(x)
    if (stringi::stri_length(x[1]) > 0) {

      is_start <- grepl('STARTING INSTRUMENT', x[1])
      is_found <- grepl('FOUND INSTRUMENT:', x[1])

      if (is_start | is_found) {
        ret <- data.frame(
          model    = as.character(parse_start('model', x)),
          serial   = as.character(parse_start('serial', x)),
          clock    = as.POSIXct(parse_start('clock', x), format = '%Y/%b/%d %H:%M:%S', tz = 'UTC'),
          host     = parse_start('host', x),
          state    = parse_start('state', x),
          used     = as.numeric(parse_start('used', x)),
          internal = parse_start('internal', x),
          stringsAsFactors = FALSE
        )
        if (is_start) {
          ret$type <- 'started'
        } else {
          ret$type <- 'connected'
        }
        return(ret)
      }
    }


  })
  aa <- rbindlist(st)

  setkey(aa, clock)
  return(aa)
}

# tmp <-parse_log(file)


