library(vctrs)
library(tidyverse)

x <- c("1970-01-01 11:12:13.123456789 Australia/Sydney" ,
       "1970-01-01 11:12:13.123456789 NZL",
       "1970-01-01 11:12:13.123456789")

enframe(sapply(x, strsplit, "\\s"))

grep("\\s", x)

# Parse Datetime ----
parse_tstp_datetime <- function(x = character(), default_tz = "UTC", pattern = NULL){
  assertthat::assert_that(is.character(x))
  if( is.null(pattern) ) {
    pattern <- "\\b[.][0-9]*\\b"
  }
  x1 <- gsub(pattern,"",x)
  x2 <- gsub("[A-Za-z/]", "", x1)
  posix_out <- unclass(as.POSIXlt(x2))

  out <- new_data_frame(x= unclass(posix_out),
                        tzone = attr(posix_out, "tzone"),
                        balanced = attr(posix_out, "balanced"),
                        class = "parsed_datetime")
  out
}

is_parsed_datetime <- function(x) inherits(x, "parsed_datetime")

check_parsed_datetime <- function(x){
  if (!is_parsed_datetime(x)){
    stop("x must be a data.frame with class `parsed_datetime`. Use function `parse_tstp_datetime")
  }
  invisible(x)
}

# Parse Subseconds ----
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
is_parsed_subsec <- function(x) inherits(x, "parsed_subsec")

check_parsed_subsec <- function(x){
  if (!is_parsed_subsec(x)){
    stop("x must be a data.frame with class `parsed_subsec`. Use function `parse_tstp_subsec")
  }
  invisible(x)
}


# Constructor for class `timestamp` -----
new_timestamp <- function(x = list(), ..., balanced = TRUE, unit = "s"){
  new_rcrd(fields = x,
           balanced = balanced,
           unit = unit,
           class = "timestamp")
}

# Constructor Helper ----
timestamp <- function(x= character(), ...){

  posix_in <- parse_tstp_datetime(x)
  balanced <- attr(posix_in, "balanced")
  subsec_in <- parse_tstp_subsec(x)
  unit <- names(subsec_in)

  out <- df_list(cbind(subsec_in, posix_in))
  new_timestamp(x = out, balanced = balanced, unit = unit)
}


tstp_unit <- function(x) { attr(x, "unit") }
tstp_balanced <- function(x) { attr(x, "balanced") }

format.timestamp <- function(x){
  unit <- tstp_unit(x)
  dat <- vec_data(x)

  out <- dat[names(dat) != unit]
  class(out) <- "POSIXlt"
  paste(as.character(as.POSIXct(out)), dat[[unit]], sep = ".")
}

vec_ptype_full.timestamp <- function(x){
  paste0("<timestamp[",tstp_unit(x),"]>")
}
vec_ptype_abbr.timestamp <- function(x){ paste0("tstp[", tstp_unit(x), "]") }

timestamp(x)
tibble::tibble(x = timestamp(x))
vec_data(timestamp(x))
attributes(timestamp(x))


x |> strsplit("\\s") |> getElement(1) |> sapply(possibly(as.Date))
|> Negate(is.na)() |> which() |> names()

hms:::parse_time(, format = "%H:%M:%OS")
strptime(as.character(x), format = "%Y-%m-%d %H:%M:%OS")
x

function(.f, otherwise = NULL, quiet = TRUE) {
  .f <- as_mapper(.f)
  force(otherwise)
  check_bool(quiet)

  function(...) {
    tryCatch(.f(...),
             error = function(e) {
               if (!quiet)
                 message("Error: ", conditionMessage(e))
               otherwise
             }
    )
  }
}
out |> typeof()

check_date <- function(x){
  out <- try(as.Date(x), silent = TRUE)
  if(is.Date(out)){
    out
  } else {
    NA_Date_
  }
}

check_date("2010-05-24")


strptime(x, "%H:%M:%S")
parse_date_time(x)
