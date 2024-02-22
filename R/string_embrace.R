#' TEmbrace charactyr vectors  with brackets
#'
#' @param x a character vector.
#' @param brackets one of `c('round', "square", "curly")`
#'`
#' @return a character vector
#' @export
#'
str_embrace <- function(x, brackets = c('round', "square", "curly")){
  br <- match.arg(brackets)



  if ( br %in% c("round", "r")){
    out <- paste0("(", x, ")")
  } else if ( br %in% c("square", "s")){
    out <- paste0("[", x, "]")
  } else if ( br %in% c("curly", "c")){
    out <- paste0("{", x, "}")
  }
  out
}
