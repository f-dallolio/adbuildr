#' New Data Types
#'
#' @name ad_type
NULL
#'
#'
## (1) new_bool -----
#' @export
#' @rdname ad_type
new_bool <- function (x = logical(), ...) {
    x <- as.logical(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::bool(), class = c(class(x),
        "ad_bool"))
}
#' @export
#' @rdname ad_type
as_bool <- new_bool
#'
#'
## (2) new_boolean -----
#' @export
#' @rdname ad_type
new_boolean <- function (x = logical(), ...) {
    x <- as.logical(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::boolean(), class = c(class(x),
        "ad_boolean"))
}
#' @export
#' @rdname ad_type
as_boolean <- new_boolean
#'
#'
## (3) new_date32 -----
#' @export
#' @rdname ad_type
new_date32 <- function (x = double(), ...) {
    x <- as.Date(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::date32(), class = c(class(x),
        "ad_date32"))
}
#' @export
#' @rdname ad_type
as_date32 <- new_date32
#'
#'
## (4) new_date64 -----
#' @export
#' @rdname ad_type
new_date64 <- function (x = double(), ...) {
    x <- as.POSIXct(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::date64(), class = c(class(x),
        "ad_date64"))
}
#' @export
#' @rdname ad_type
as_date64 <- new_date64
#'
#'
## (5) new_float -----
#' @export
#' @rdname ad_type
new_float <- function (x = double(), ...) {
    x <- as.double(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::float(), class = c(class(x),
        "ad_float"))
}
#' @export
#' @rdname ad_type
as_float <- new_float
#'
#'
## (6) new_float16 -----
#' @export
#' @rdname ad_type
new_float16 <- function (x = double(), ...) {
    x <- as.double(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::float16(), class = c(class(x),
        "ad_float16"))
}
#' @export
#' @rdname ad_type
as_float16 <- new_float16
#'
#'
## (7) new_float32 -----
#' @export
#' @rdname ad_type
new_float32 <- function (x = double(), ...) {
    x <- as.double(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::float32(), class = c(class(x),
        "ad_float32"))
}
#' @export
#' @rdname ad_type
as_float32 <- new_float32
#'
#'
## (8) new_float64 -----
#' @export
#' @rdname ad_type
new_float64 <- function (x = double(), ...) {
    x <- as.double(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::float64(), class = c(class(x),
        "ad_float64"))
}
#' @export
#' @rdname ad_type
as_float64 <- new_float64
#'
#'
## (9) new_halffloat -----
#' @export
#' @rdname ad_type
new_halffloat <- function (x = double(), ...) {
    x <- as.double(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::halffloat(), class = c(class(x),
        "ad_halffloat"))
}
#' @export
#' @rdname ad_type
as_halffloat <- new_halffloat
#'
#'
## (10) new_int16 -----
#' @export
#' @rdname ad_type
new_int16 <- function (x = integer(), ...) {
    x <- as.integer(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::int16(), class = c(class(x),
        "ad_int16"))
}
#' @export
#' @rdname ad_type
as_int16 <- new_int16
#'
#'
## (11) new_int32 -----
#' @export
#' @rdname ad_type
new_int32 <- function (x = integer(), ...) {
    x <- as.integer(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::int32(), class = c(class(x),
        "ad_int32"))
}
#' @export
#' @rdname ad_type
as_int32 <- new_int32
#'
#'
## (12) new_int64 -----
#' @export
#' @rdname ad_type
new_int64 <- function (x = double(), ...) {
    x <- bit64::as.integer64(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::int64(), class = c(class(x),
        "ad_int64"))
}
#' @export
#' @rdname ad_type
as_int64 <- new_int64
#'
#'
## (13) new_int8 -----
#' @export
#' @rdname ad_type
new_int8 <- function (x = integer(), ...) {
    x <- as.integer(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::int8(), class = c(class(x),
        "ad_int8"))
}
#' @export
#' @rdname ad_type
as_int8 <- new_int8
#'
#'
## (14) new_large_utf8 -----
#' @export
#' @rdname ad_type
new_large_utf8 <- function (x = character(), ...) {
    x <- as.character(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::large_utf8(), class = c(class(x),
        "ad_large_utf8"))
}
#' @export
#' @rdname ad_type
as_large_utf8 <- new_large_utf8
#'
#'
## (15) new_string -----
#' @export
#' @rdname ad_type
new_string <- function (x = character()) {
    x <- as.character(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::string(x = , encoding = NULL),
        class = c(class(x), "ad_string"))
}
#' @export
#' @rdname ad_type
as_string <- new_string
#'
#'
## (16) new_uint16 -----
#' @export
#' @rdname ad_type
new_uint16 <- function (x = integer(), ...) {
    x <- as.integer(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::uint16(), class = c(class(x),
        "ad_uint16"))
}
#' @export
#' @rdname ad_type
as_uint16 <- new_uint16
#'
#'
## (17) new_uint32 -----
#' @export
#' @rdname ad_type
new_uint32 <- function (x = integer(), ...) {
    x <- as.integer(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::uint32(), class = c(class(x),
        "ad_uint32"))
}
#' @export
#' @rdname ad_type
as_uint32 <- new_uint32
#'
#'
## (18) new_uint64 -----
#' @export
#' @rdname ad_type
new_uint64 <- function (x = integer(), ...) {
    x <- as.integer(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::uint64(), class = c(class(x),
        "ad_uint64"))
}
#' @export
#' @rdname ad_type
as_uint64 <- new_uint64
#'
#'
## (19) new_uint8 -----
#' @export
#' @rdname ad_type
new_uint8 <- function (x = integer(), ...) {
    x <- as.integer(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::uint8(), class = c(class(x),
        "ad_uint8"))
}
#' @export
#' @rdname ad_type
as_uint8 <- new_uint8
#'
#'
## (20) new_utf8 -----
#' @export
#' @rdname ad_type
new_utf8 <- function (x = character(), ...) {
    x <- as.character(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::utf8(), class = c(class(x),
        "ad_utf8"))
}
#' @export
#' @rdname ad_type
as_utf8 <- new_utf8
#'
#'
## (21) new_dictionary -----
#' @export
#' @rdname ad_type
new_dictionary <- function (x = integer(), index_type = int32(), value_type = utf8(),     ordered = FALSE)
{
    x <- as_factor(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::dictionary(index_type = int32(),
        value_type = utf8(), ordered = FALSE), class = c(class(x),
        "ad_dictionary"))
}
#' @export
#' @rdname ad_type
as_dictionary <- new_dictionary
#'
#'
## (22) new_decimal -----
#' @export
#' @rdname ad_type
new_decimal <- function (x = double(), precision, scale) {
    x <- as.double(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::decimal(precision = ,
        scale = ), class = c(class(x), "ad_decimal"))
}
#' @export
#' @rdname ad_type
as_decimal <- new_decimal
#'
#'
## (23) new_decimal128 -----
#' @export
#' @rdname ad_type
new_decimal128 <- function (x = double(), precision, scale) {
    x <- as.double(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::decimal128(precision = ,
        scale = ), class = c(class(x), "ad_decimal128"))
}
#' @export
#' @rdname ad_type
as_decimal128 <- new_decimal128
#'
#'
## (24) new_decimal256 -----
#' @export
#' @rdname ad_type
new_decimal256 <- function (x = double(), precision, scale) {
    x <- as.double(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::decimal256(precision = ,
        scale = ), class = c(class(x), "ad_decimal256"))
}
#' @export
#' @rdname ad_type
as_decimal256 <- new_decimal256
#'
#'
## (25) new_duration -----
#' @export
#' @rdname ad_type
new_duration <- function (x = double(), unit = c("s", "ms", "us", "ns")) {
    x <- as.difftime(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::duration(unit = c("s",
        "ms", "us", "ns")), class = c(class(x), "ad_duration"))
}
#' @export
#' @rdname ad_type
as_duration <- new_duration
#'
#'
## (26) new_fixed_size_list_of -----
#' @export
#' @rdname ad_type
new_fixed_size_list_of <- function (x = list(), type, list_size) {
    x <- as_list_of(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::fixed_size_list_of(type = ,
        list_size = ), class = c(class(x), "ad_fixed_size_list_of"))
}
#' @export
#' @rdname ad_type
as_fixed_size_list_of <- new_fixed_size_list_of
#'
#'
## (27) new_large_list_of -----
#' @export
#' @rdname ad_type
new_large_list_of <- function (x = list(), type) {
    x <- as_list_of(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::large_list_of(type = ),
        class = c(class(x), "ad_large_list_of"))
}
#' @export
#' @rdname ad_type
as_large_list_of <- new_large_list_of
#'
#'
## (28) new_list_of -----
#' @export
#' @rdname ad_type
new_list_of <- function (x = list(), ..., .ptype = NULL) {
    x <- as_list_of(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::list_of(..., .ptype = NULL),
        class = c(class(x), "ad_list_of"))
}
#' @export
#' @rdname ad_type
as_list_of <- new_list_of
#'
#'
## (29) new_time32 -----
#' @export
#' @rdname ad_type
new_time32 <- function (x = double(), unit = c("ms", "s")) {
    x <- hms::as_hms(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::time32(unit = c("ms",
        "s")), class = c(class(x), "ad_time32"))
}
#' @export
#' @rdname ad_type
as_time32 <- new_time32
#'
#'
## (30) new_time64 -----
#' @export
#' @rdname ad_type
new_time64 <- function (x = double(), unit = c("ns", "us")) {
    x <- hms::as_hms(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::time64(unit = c("ns",
        "us")), class = c(class(x), "ad_time64"))
}
#' @export
#' @rdname ad_type
as_time64 <- new_time64
#'
#'
## (31) new_timestamp -----
#' @export
#' @rdname ad_type
new_timestamp <- function (x = double(), unit = c("s", "ms", "us", "ns"), timezone = "") {
    x <- as.POSIXct(x)
    vctrs::new_vctr(
       .data =  x,
       arrow = arrow::timestamp(unit = c("s",
        "ms", "us", "ns"), timezone = ""), class = c(class(x),
        "ad_timestamp"))
}
#' @export
#' @rdname ad_type
as_timestamp <- new_timestamp
#'
#'
