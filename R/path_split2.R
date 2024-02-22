#' Split Paths
#'
#' @name path_split2
NULL
#'
#' @export
#' @rdname path_split2
path_split2 <- function(x, ext_remove = FALSE){
  if(ext_remove) {
    x <- fs::path_ext_remove(x)
  }
  out <- fs::path_split(x)
  if ( purrr::is_scalar_atomic(x) ) {
    return(out[[1]])
  }
  out
}
#'
#' @export
#' @rdname path_split2
path_split2_noext <- function(x){
  path_split2(x, ext_remove = TRUE)
}