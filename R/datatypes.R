#' Data Types
#'
#' @param x vector.
#'
#' @name data_types
NULL
#'
#' @rdname data_types
#' @export
new_int64 <- function(x){
  x <- bit64::as.integer64(x)
  structure(.data = x,
           # base = quote('bit64::integer64'),
           arrow = arrow::int64(),
           class = c(class(x), "ad_type"))
}
#'
#' @rdname data_types
#' @export
new_int32 <- function(x){
  x <- as.integer(x)
  structure(.data = x,
           # base = 'integer',
           arrow = arrow::int32(),
           class = c(class(x), "ad_type"))
}
#'
#' @rdname data_types
#' @export
new_utf8 <- function(x){
  x <- as.character(x)
  structure(.data = x,
           base = 'character',
           arrow = "utf8()",
           class = c(class(x), "ad_type"))
}
#'
#' @rdname data_types
#' @export
new_date32 <- function(x){
  x <- as.Date(x)
  structure(.data = x,
           base = 'Date',
           arrow = "date32()",
           class = c(class(x), "ad_type"))
}
#'
#' @rdname data_types
#' @export
new_time32 <- function(x){
  x <- hms::as_hms(x)
  structure(.data = x,
           base = 'hms::hms',
           arrow = "time32",
           class = c(class(x), "ad_type"))
}
#'
#' @rdname data_types
#' @export
new_datetime <- function(x){
  x <- as.POSIXct(x)
  structure(.data = x,
           base = 'POSIXct',
           arrow = "datetime",
           class = c(class(x), "ad_type"))
}
#'
#' @rdname data_types
#' @export
new_duration <- function(x){
  x <- as.difftime(x)
  structure(.data = x,
           base = 'difftime',
           arrow = "duration",
           class = c(class(x), "ad_type"))
}
#'
#' @rdname data_types
#' @export
new_float64 <- function(x){
  x <- as.numeric(x)
  structure(.data = x,
           base = 'double',
           arrow = "float32",
           class = c(class(x), "ad_type"))
}
#'
#' @rdname data_types
#' @export
new_boolean <- function(x){
  x <- as.logical(x)
  structure(.data = x,
           base = 'logical',
           arrow = "boolean",
           class = c(class(x), "ad_type"))
}
