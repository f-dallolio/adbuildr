# library(vctrs)
# library(tidyverse)
#
# x <- c("2010-01-01 11:12:13.123456789 Australia/Sydney" ,
#        "1998-01-01 11:12:13.123456 NZL",
#        "1975-01-01 11:12:13",
#        "12JUNE2010 10:10:10")
#
#

.tstamp_df <- function(x){
  xx <- unlist(strsplit(x, "\\s"))

  ## date -----
  x_date0 <- as.character(parse_date(xx))
  id_date <- !is.na(x_date0)
  if( any(id_date) ){
    x_date <- x_date0[id_date]
  } else {
    x_date <- NA
  }
  ## time ----
  x_time0 <- as.character(parse_time(xx))
  id_time <- !is.na(x_time0)

  ## subsec ---
  if( any(!id_time) ){
    x_time <- x_time0[id_time]
    subsec0 <- gsub(x_time, "", xx[id_time])
    n <- nchar(subsec0)
    if(n > 0){
      times <- min(c(9, n-1))
      unit <- rep(c("ms", "us", "ns"), each = 3)[times]
      subsec <- as.integer(substr(subsec0, 2, max(c(9, times))))
    } else {
      times <- 0
      subsec <- 0
      unit <- "s"
    }
  } else {
    x_time <- NA
    times <- NA
    subsec <- NA
    unit <- NA
  }

  ## x_left -----
  id_left <-(!id_time & !id_date)
  if( any(id_left) ) {
    if( !is.na(x_date) & !is.na(x_time)){
      check_tz <- paste(xx[id_left])
      x_left <- NA
    } else {
      check_tz <- NA
      x_left <- paste(xx[id_left])
    }
  } else {
    check_tz <- ""
    x_left <- NA

  }
  check_tz
  x_left

  ## output ----
  res <- df_list(date = x_date, time = x_time, subsec = subsec, times = times, unit = unit,
                 check_tz = check_tz, x_left = x_left)
  new_data_frame(res)
}

tstamp_df <- function(x, quiet = TRUE){
  if(quiet){
    res <- suppressWarnings(lapply(x, .tstamp_df ))
  } else {
    res <- lapply(x, .tstamp_df )
  }
  do.call("rbind", res)
}

tstamp_df(x)
