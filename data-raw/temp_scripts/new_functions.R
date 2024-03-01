as_double <- function(x, strict = TRUE) {
  out <- suppressWarnings(as.double(x))
  if( strict ) stopifnot("x cannot be coerced to double" = all(!is.na(as.double(x))))
  out
}
as_numeric <- function(x, strict = TRUE) {
  out <- suppressWarnings(as.numeric(x))
  if( strict ) stopifnot("x cannot be coerced to double" = all(!is.na(as.double(x))))
  out
}
as_integer <- function(x, strict = TRUE) {
  out <- suppressWarnings(as.integer(x))
  if( strict ) stopifnot("x cannot be coerced to integer" = all(!is.na(out)))
  out
}

Date <- vctrs::new_date
POSIXct <- function(x = double(), tzone = "UTC") vctrs::new_datetime(x, tzone)
duration <- function(x = double(), units = "secs") vctrs::new_duration(x, units)

new_vector <- function(x){
  out <- switch((x),
         "integer" = integer(),
         "integer64" = bit64::integer64(),
         "numeric" = numeric(),
         "character" = character(),
         "logical" = logical(),
         "Date" = Date(),
         "hms" = hms::hms(),
         "difftime" = duration(),
         "POSIXct" = POSIXct(),
         "list" = list() )
  out
}

convert_to_arrow <- function( x, .vector_in, .eval){
  if( .vector_in ) x <- class(x)[[1]]
  out <- switch(x,
                "integer" = rlang::call2("int32", .ns = "arrow"),
                "integer64" = rlang::call2("int64"), .ns = "arrow",
                "numeric" = rlang::call2("float64", .ns = "arrow"),
                "character" = rlang::call2("utf8", .ns = "arrow"),
                "logical" = rlang::call2("boolean", .ns = "arrow"),
                "Date" = rlang::call2("date32", .ns = "arrow"),
                "hms" = rlang::call2("time32", unit = "s", .ns = "arrow"),
                "difftime" <- rlang::call2("duration", unit = "us", .ns = "arrow"),
                "POSIXct" = rlang::call2("timestamp",
                                         unit = "us",
                                         timezone = "UTC", .ns = "arrow")
  )
  if( .eval ){
    eval(out)
  } else {
    out
  }
}
new_arrow_chr <- function(x){
  stopifnot( is.character(x) )
  convert_to_arrow(x, .vector_in = FALSE, .eval = TRUE)
}
new_arrow <- function(x){
  stopifnot( is.vector(x) | is.list(x) | is.data.frame(x) )
  convert_to_arrow(x, .vector_in = TRUE, .eval = TRUE)
}



arrow_schema_to_type <- function(x){
  stopifnot( inherits(x, "Schema") && inherits(x, "ArrowObject"))
  out <- lapply(as.list(x), purrr::pluck, "type")
  setNames(out, names(x))
}


is_time <- function(x = character()){
  vctrs::vec_assert(x, character())
  out <- grepl("^[0-9]{2}[.:-][0-9]{2}[.:-][0-9]{2}", x[!is.na(x)])
  if( !all(out) ){
    rws <- which(!out)
    rrws <- stringr::str_flatten_comma(rws)#, last = ", and ")
    warning(paste0("Elements in rows c(", rrws, ") are not formatted as `time`."))
  }
  all(out)
}
check_time <- function(x = character()){
  stopifnot(is_time(x))
  invisible(x)
}



is_time_frac <- function(x = character()){
  check_time(x)
  any(grepl("[.][0-9]*", x))
}
check_time_frac <- function(x){
  stopifnot(is_time_frac(x))
}
x <- c("12:12:12.00", "12:12:12", "12:12:12.0000")

new_time <- function(x = character()){
  check_time(x)
  unit <- "s"
  x2 <- rep("", length(x))
  unit_id <- 1
  x <- strsplit(x, "\\.")
  frac_sec <- sapply(x, length) > 1
  if( length(frac_sec) != 0 ){
    x2[frac_sec] <- sapply(x[frac_sec], getElement, 2) |>
      stringr::str_sub(1,9)
    xlen <-  3 * ceiling( max(stringr::str_length(x2)) / 3)
    x2 <- unlist(stringr::str_pad(x2, 9, "right", "0"))
    unit_id <- unit_id + min(ceiling( xlen / 3), 3)
  }
  x1 <- sapply(x, getElement, 1) |> hms::as_hms()
  unit = c(time32 ="s", time32 = "ms", time64 = "us", time64 = "ns")[unit_id]
  out <- paste(x1, x2, ".")
  new_class <- names(unit)
  vctrs::new_vctr(.data = hms::as_hms(x1), sec_frac = x2, unit = unit, class = c(new_class, "hms"))
}

new_time(x)

vec_ptype_full.time32 <- function(x){
  paste0(class(x)[[1]], "[", unit(x), "]")
}

data.frame(x = x) |>
  mutate(tidyr:::extract())

x <- "10:11:12"
pattern <- "(^[0-2][0-9]\\b)[:.-](\\b[0-5][0-9]\\b)[:.-](\\b[0-5][0-9]$)"
proto <- data.frame(chr=integer(), start=integer(), end=integer())
strcapture(pattern, x, proto)
strcapture()

hh <- "[0-2][0-9]"
mm <- ss <- "[0-5][0-9]"

unclass(strptime("10:11:12", "%T"))[c("sec", "min","hour")]

xx <- c(x, "10:11:12.1234567891")

  do.call("rbind",
          lapply(strsplit(xx,"[:.-]") ,
         \(x) setNames(x[1:3], c("H","M","S")))) |>
    as.data.frame()


([0-20-9]+)[:./-]([0-50-9]+)[:./-]([0-50-9]+)[.]{0-1}([0-9]{0,9})

x  <- c("10:10:10")
pattern <-  "(^\\d{2}\\b)[:.-](\\b\\d{2}\\b)[:.-](\\b\\d{2})"
m <- regexpr(pattern, x, perl = TRUE)
stringr::str_extract(x, pattern)

strftime("10:10:10", format = %T)
as.POSIXlt(10)
format(Sys.time(), "%H:%M:%OS6")
format(as.POSIXct(1000), "%H:%M:%OS6")

.xx <- c("10:11:12",
         "10:11:12.000000001",
         "10:11:12.000000100",
         "10:11:12.000001000",
         "10:11:12.000010000",
         "10:11:12.000100000",
         "10:11:12.001000000",
         "10:11:12.010000000",
         "10:11:12.100000000")

x <- .xx

any(f0 <- c("", "a") == "")
f0



x <- unclass(as.POSIXlt(x))
sec_frac <- x$sec%%1
ten2pow <- function(x, y) as.integer(x*10L^as.integer(y))
ns <-ten2pow(sec_frac,6)*10^-3
us <- ten2pow(ns%%1,6)*10^-3


  x1 <- (( * 10^9))%/%10^6
x2 <- ((x$sec%%1 * 10^9 -   x1))%/%10^3

