library(vctrs)
library(tidyverse)

parse_tstp_subsec <- function(x = character(), pattern = NULL){
  assertthat::assert_that(is.character(x), msg = "x must be a `character vector")
  if( is.null(pattern) ) {
    pattern <- "\\b[.][0-9]*\\b"
  }
  fsec_id <- grepl(pattern, x)
  fsec0 <- rep("", length(fsec_id))
  fsec0[fsec_id] <- regmatches(x, regexpr(pattern, x))
  if( all(fsec0 == "") ){
    tot <- ns <- us <- ms <- rep(0, length(x))

  } else {
    fsec <- stringr::str_pad(
      string = substr(fsec0, 2, 10),
      width = 9,
      side = "right",
      pad = "0")
    ns <-  as.integer(substr(fsec, 7, 9))
    us <-  as.integer(substr(fsec, 4, 6))
    ms  <-  as.integer(substr(fsec, 1, 3))
    # tot <- ns + us * 1000 + ms * 1000000
  }
  fsec_out <- df_list(
    ns = ns,
    us = us,
    ms = ms
  )
  unit_id <- which(sapply(fsec_out, function(x) any(x != 0)))
  if (length(unit_id) == 0) {
    unit <- "s"
  } else {
    unit <-  names(fsec_out)[min(unit_id)]
  }

  unit_out <- switch(unit,
                     "ns" = ms * 1000000 + us * 1000 + ns,
                     "us" = ms * 1000000 + us * 1000,
                     "ms" = ms)
  out <-setNames(list(unit_out), unit)

  new_data_frame(x = out,
                 class = "parsed_subsec")
}

match_tz <- function(x, cutoff = 0.5, show = 1){
  name <- OlsonNames()
  value <- stringdist::stringsim(x, name)
  names(value) <- name
  value_id <-value > cutoff
  value <- sort(value[value_id], decreasing = TRUE)

  perfect_match <- any(value == 1)
  no_match <- all( value < cutoff)
  if(no_match) {
    res <-NA_character_
    return(res)
  }
  if(perfect_match) {
    out <- value[value == 1]
    n <- length(out)
    id <- min(c(show, n))
    match <-  out[seq(1, id)]
    nms <- names(match)
    res <- rep("100%", length(match))
    names(res) <- nms
    return(res)
  }
  out <- value
  n <- length(out)
  id <- min(c(show, n))
  match <-  out[seq(1, id)]
  nms <- names(match)
  res <- paste0(round(match*100, 1), "%")
  names(res) <- nms
  res
}

.check_date_names <- function(x){
  date_names <- unnest(tibble::enframe((unclass(date_names_lang("en")))), value)
  date_names <- filter(date_names, name != "am_pm")
  nms <- tolower(date_names$value)
  names(nms) <- nms
  flag <- unlist(sapply(nms, grepl, x = tolower(x)))# value = TRUE))
  if( any(flag) ){
    NULL
  } else {
    x
  }
}

check_date_names <- function(x){
  out <- sapply(x, .check_date_names, USE.NAMES = FALSE)
  has_date_names <- is.na(out)
  if( any(has_date_names) ){
    dtnames <- paste(which(has_date_names), collapse = ", ")
    if(length(dtnames) == 1){
      warning("This string uses date_names: c(", dtnames,")")
    } else {
      warning("These strings use date_names: c(", dtnames,")")
    }
  }
  out
}

has_date_names <- function(x){
  date_names <- unnest(tibble::enframe((unclass(date_names_lang("en")))), value)
  date_names <- filter(date_names, name != "am_pm")
  nms <- tolower(date_names$value)
  names(nms) <- nms
  flag <- unlist(sapply(nms, grepl, x = tolower(x)))# value = TRUE))
  any(flag)
}

match_date <- function(x, first_out = TRUE){
  out <- try(as.Date(x), silent = TRUE)
  if(is.Date(out)){
    as.character(out)
  } else {
    NA_character_
  }
}

match_time <- function(x){
  out <- rep(NA, length(x))
  tme <- suppressWarnings(as.character(parse_time(x)))
  id <- which(!is.na(tme))
  subsec <- parse_tstp_subsec(x[id])
  out[id] <-tme[id]

  out
}

x <- c("1970-01-01 11:12:13.123456789 Australia/Sydney" ,
       "1970-01-01 11:12:13.123456 NZL",
       "1970-01-01 11:12:13",
       "12JUNE2010 10:10:10")
f <- function(x){
  # x <- x[[3]]
  if(has_date_names(x)){
    return(NULL)
  }
  xx <- unlist(strsplit(x, "\\s"))

  x_date <- match_date(xx)
  date_id <- which(!is.na(x_date))
  x_date <- x_date[date_id]

  x_tz_pct <- sapply(xx, match_tz, USE.NAMES = FALSE)
  tz_id <- which(!is.na(x_tz_pct))

  x_time <- match_time(xx)
  time_id <- which(!is.na(x_time))
  x_time <- x_time[time_id]

  x_subsec = parse_tstp_subsec(xx[time_id])
  if(length(x_subsec[[1]]) == 0){
    x_unit <- "s"
    x_subsec = paste(rep("0", 9), collapse = "")
  } else {
    x_unit <- names(x_subsec)
    x_subsec = x_subsec[[x_unit]]
  }
  times <- switch(x_unit,
                 "ns" = 9,
                 "us" = 6,
                 "ms" = 3,
                 "s" = 0)
  x_subsec = as.integer(x_subsec) / 10^(9-times)

  if(length(xx) == 2){
    x_left <- ""
  } else {
    x_left <- xx[-c(date_id, time_id)]
  }

  out <- df_list(date = x_date,
                 time = x_time,
                 x_left = x_left,
                 # tz = x_tz,
                 # tz0 = x_tz0,
                 subsec = x_subsec,
                 times = 10^-(times), unit = x_unit)
  new_data_frame(lapply(out, unname))
}

xxxx <- lapply(x, f)
xxxx
xxxx$subsec |> class()




