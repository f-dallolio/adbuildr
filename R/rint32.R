#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name r_types
NULL

## R(int32) ----

new_rint32 <- function(x = integer()) {
  if (!is.integer(x)) {
    rlang::abort("`x` must be an integer vector.")
  }
  vctrs::new_vctr(x,
                  arrow = arrow::int32(),
                  class = "rint32")
}

# for compatibility with the S4 system
methods::setOldClass(c("rint32", "vctrs_vctr"))

#' `rint32` vector
#'
#' This creates an integer vector that includes the equivalent arrow type (`int32`)
#' as attribute.
#'
#' @param x A numeric vector.
#' @return An S3 vector of class `rint32`.
#'
#' @rdname r_types
#' @export
#' @examples
#' x <- rint32(1:10)
#' x
#' attributes(x)
rint32 <- function(x = numeric()) {
  x <- vctrs::allow_lossy_cast(vctrs::vec_cast(x, integer()))
  new_rint32(x)
}
#' @export
#' @rdname r_types
is_rint32 <- function(x) {
  inherits(x, "rint32")
}
#' @export
#' @rdname r_types
format.rint32 <- function(x, ...) {
  out <- formatC(vctrs::vec_data(x))
  out[is.na(x)] <- NA
  # out[!is.na(x)] <- paste0(out[!is.na(x)], "L")
  out
}
#' @export
#' @rdname r_types
vec_ptype2.rint32.rint32 <- function(x, y, ...) new_rint32()
#' @export
#' @rdname r_types
vec_ptype2.rint32.integer <- function(x, y, ...) new_rint32()
#' @export
#' @rdname r_types
vec_ptype2.integer.rint32 <- function(x, y, ...) new_rint32()
#' @export
#' @rdname r_types
vec_ptype2.rint32.double <- function(x, y, ...) new_rint32()
#' @export
#' @rdname r_types
vec_ptype2.double.rint32 <- function(x, y, ...) new_rint32()

#' @export
#' @rdname r_types
vec_cast.rint32.rint32 <- function(x, to, ...) x
#' @export
#' @rdname r_types
vec_cast.rint32.integer <- function(x, to, ...) rint32(x)
#' @export
#' @rdname r_types
vec_cast.rint32.rint32 <- function(x, to, ...) vec_data(x)
#' @export
#' @rdname r_types
vec_cast.rint32.double <- function(x, to, ...) rint32(x)
#' @export
#' @rdname r_types
vec_cast.double.rint32 <- function(x, to, ...) vec_data(x)

#' @export
#' @rdname r_types
as_rint32 <- function(x, ...) {
  UseMethod("as_rint32")
}
#' @export
#' @rdname r_types
as_rint32.default <- function(x, ...) {
  vec_cast(x, new_rint32())
}
#' @export
#' @rdname r_types
as_rint32.character <- function(x) {
  as_num <- suppressWarnings( as.integer(x) )
  if ( any(is.na(as_num)) ){
    x
  } else {
    rint32(as_num)
  }
}


## R(float64) ----

new_rfloat64 <- function(x = double()) {
  if (!is.double(x)) {
    rlang::abort("`x` must be an double vector.")
  }
  vctrs::new_vctr(x,
                  arrow = arrow::float64(),
                  class = "rfloat64")
}
# for compatibility with the S4 system
methods::setOldClass(c("rfloat64", "vctrs_vctr"))

#' `rfloat64` vector
#'
#' This creates a double vector that includes the equivalent arrow type (`float64`)
#' as attribute.
#'
#' @param x A numeric vector.
#' @return An S3 vector of class `float32`.
#'
#' @rdname r_types
#' @export
#' @examples
#' x <- rfloat64(1:10)
#' x
#' attributes(x)
rfloat64 <- function(x = numeric()) {
  x <- vctrs::vec_cast(x, double())
  new_rfloat64(x)
}
#' @export
#' @rdname r_types
is_rfloat64 <- function(x) {
  inherits(x, "rfloat64")
}
#' @export
#' @rdname r_types
format.rfloat64 <- function(x, ...) {
  out <- formatC(vctrs::vec_data(x))
  out[is.na(x)] <- NA
  # out[!is.na(x)] <- paste0(out[!is.na(x)], "L")
  out
}
#' @export
#' @rdname r_types
vec_ptype2.rfloat64.rfloat64 <- function(x, y, ...) new_rfloat64()
#' @export
#' @rdname r_types
vec_ptype2.rfloat64.integer <- function(x, y, ...) new_rfloat64()
#' @export
#' @rdname r_types
vec_ptype2.integer.rfloat64 <- function(x, y, ...) new_rfloat64()
#' @export
#' @rdname r_types
vec_ptype2.rfloat64.double <- function(x, y, ...) new_rfloat64()
#' @export
#' @rdname r_types
vec_ptype2.double.rfloat64 <- function(x, y, ...) new_rfloat64()

#' @export
#' @rdname r_types
vec_cast.rfloat64.rfloat64 <- function(x, to, ...) x
#' @export
#' @rdname r_types
vec_cast.rfloat64.integer <- function(x, to, ...) rfloat64(x)
#' @export
#' @rdname r_types
vec_cast.integer.rfloat64 <- function(x, to, ...) vec_data(x)
#' @export
#' @rdname r_types
vec_cast.rfloat64.double <- function(x, to, ...) rfloat64(x)
#' @export
#' @rdname r_types
vec_cast.double.rfloat64 <- function(x, to, ...) vec_data(x)

#' @export
#' @rdname r_types
as_rfloat64 <- function(x, ...) {
  UseMethod("as_rfloat64")
}
#' @export
#' @rdname r_types
as_rfloat64.default <- function(x, ...) {
  vec_cast(x, new_rfloat64())
}
#' @export
#' @rdname r_types
as_rfloat64.character <- function(x) {
  as_num <- suppressWarnings( as.numeric(x) )
  if ( any(is.na(as_num)) ){
    x
  } else {
    rfloat64(as_num)
  }
}

## R(utf8) ----

new_rutf8 <- function(x = character()) {
  if (!is.character(x)) {
    rlang::abort("`x` must be an character vector.")
  }
  vctrs::new_vctr(x,
                  arrow = arrow::utf8(),
                  class = c("rutf8", class(x)))
}
# for compatibility with the S4 system
methods::setOldClass(c("rutf8", "vctrs_vctr"))

#' `rutf8` vector
#'
#' This creates a character vector that includes the equivalent arrow type (`utf8`)
#' as attribute.
#'
#' @param x A numeric vector.
#' @return An S3 vector of class `rutf8`.
#'
#' @rdname r_types
#' @export
#' @examples
#' x <- rutf8(1:10)
#' x
#' attributes(x)
rutf8 <- function(x = character()) {
  x <- vctrs::vec_cast(x, character())
  new_rutf8(x)
}
#' @export
#' @rdname r_types
is_rutf8 <- function(x) {
  inherits(x, "rutf8")
}
#' @export
#' @rdname r_types
format.rutf8 <- function(x, ...) {
  out <- formatC(vctrs::vec_data(x))
  out[is.na(x)] <- NA
  out[!is.na(x)] <- paste0('"',out[!is.na(x)], '"')
  out
}
#' @export
#' @rdname r_types
vec_ptype2.rutf8.rutf8 <- function(x, y, ...) new_rutf8()
#' @export
#' @rdname r_types
vec_ptype2.rutf8.character <- function(x, y, ...) new_rutf8()
#' @export
#' @rdname r_types
vec_ptype2.character.rutf8 <- function(x, y, ...) new_rutf8()
#' @export
#' @rdname r_types
vec_ptype2.rutf8.integer <- function(x, y, ...) new_rutf8()
#' @export
#' @rdname r_types
vec_ptype2.integer.rutf8 <- function(x, y, ...) new_rutf8()
#' @export
#' @rdname r_types
vec_ptype2.rutf8.double <- function(x, y, ...) new_rutf8()
#' @export
#' @rdname r_types
vec_ptype2.double.rutf8 <- function(x, y, ...) new_rutf8()

#' @export
#' @rdname r_types
vec_cast.rutf8.rutf8 <- function(x, to, ...) x
#' @export
#' @rdname r_types
vec_cast.rutf8.character <- function(x, to, ...) rutf8(x)
#' @export
#' @rdname r_types
vec_cast.character.rutf8 <- function(x, to, ...) vec_data(x)
#' @export
#' @rdname r_types
vec_cast.rutf8.integer <- function(x, to, ...) new_rutf8(as.character(x))
#' @export
#' @rdname r_types
vec_cast.integer.rutf8 <- function(x, to, ...) as_rint32(x) |> as.integer()
#' @export
#' @rdname r_types
vec_cast.rutf8.double <- function(x, to, ...) new_rutf8(as.character(x))
