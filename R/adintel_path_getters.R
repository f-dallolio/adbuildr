#' Adintel Paths Getters
#'
#' @name adintel_path_getters
NULL
#'
#' @export
#' @rdname adintel_path_getters
get_static_paths <- function(x){
  if ( !is_adintel_path(x) ){
    x <- ad_path(x)
  }
  id <- is.na(field(x, "year"))
  x[[id]]
}
#' @export
#' @rdname adintel_path_getters
get_dynamic_paths <- function(x, .class = c("occ", "imp", "ue", "ref")){
  if ( !is_adintel_path(x) ){
    x <- ad_path(x)
  }
  .args <- match.arg(.class, several.ok = TRUE)
  id <- !is.na(field(x, "year"))
  out <- x[id]
  id_out <- vec_data(out)$file_type %in% .args
  out[id_out]
}
#' @export
#' @rdname adintel_path_getters
get_occ_paths <- function(x){
  get_dynamic_paths(x, .class = "occ")
}
#' @export
#' @rdname adintel_path_getters
get_imp_paths <- function(x){
  get_dynamic_paths(x, .class = "imp")
}
#' @export
#' @rdname adintel_path_getters
get_ue_paths <- function(x){
  get_dynamic_paths(x, .class = "ue")
}
#' @export
#' @rdname adintel_path_getters
get_ref_paths <- function(x){
  id <- field(x, "file_type") == "ref"
  x[[id]]
}
#'
#'
## Fields Getters ----
#'
#' @export
#' @rdname adintel_path_getters
get_path <- function(x){
  assertthat::assert_that(is_adintel_path(x))
  field(x, "path")
}
#' @export
#' @rdname adintel_path_getters
get_file_type <- function(x){
  assertthat::assert_that(is_adintel_path(x))
  field(x, "file_type")
}
#' @export
#' @rdname adintel_path_getters
get_year <- function(x){
  assertthat::assert_that(is_adintel_path(x))
  field(x, "year")
}
#' @export
#' @rdname adintel_path_getters
get_tbl_class <- function(x){
  assertthat::assert_that(is_adintel_path(x))
  field(x, "tbl_class")
}
#' @export
#' @rdname adintel_path_getters
get_tbl_type <- function(x){
  assertthat::assert_that(is_adintel_path(x))
  field(x, "tbl_type")
}
#' @export
#' @rdname adintel_path_getters
get_tbl <- function(x){
  assertthat::assert_that(is_adintel_path(x))
  field(x, "tbl")
}
#' @export
#' @rdname adintel_path_getters
common_path <- function(x){
  assertthat::assert_that(is_adintel_path(x))
  attr(x, "common_path")
}
