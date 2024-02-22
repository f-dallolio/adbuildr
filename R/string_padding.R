#' String Padding
#'
#' @name string_padding
NULL#'
#'
#' @rdname string_padding
#' @export
str_rpad <- function(x, n = 2, max = T){
  if(max){
    str_pad(x, width = max(nchar(x))+n, side = "right", pad = " ")
  } else{
    str_pad(x, width = nchar(x) + n, side = "right", pad = " ")
  }
}
#'
#' @rdname string_padding
#' @export
str_lpad <- function(x, n = 2, max = T){
  if(max){
    str_pad(x, width = max(nchar(x))+n, side = "left", pad = " ")
  } else {
    str_pad(x, width = nchar(x) + n, side = "left", pad = " ")
  }
}
