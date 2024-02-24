#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name r_types
NULL

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
#' This creates an integer vector that with the equivalent arrow type (`int32`)
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
  as_num <- suppressWarnings( as.numeric(x) )
  if ( any(is.na(as_num)) ){
    x
  } else {
    rint32(as_num)
  }
}


