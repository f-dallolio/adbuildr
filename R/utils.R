#' Utilities
#'
#' @name utils
NULL
#'
#' @export
#' @rdname utils
not <- function(x) {
  !x
}
#'
#' @export
#' @rdname utils
get_element <- function(x, id){
  n <- vctrs::vec_size(x)
  id[id<0] <- sort(n + 1 + id[id<0])
  x[id]
}