#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name class_adintel_path
NULL

new_ad_path <- function(path, file_type, year,
                        tbl_class, tbl_type, tbl,
                        common_path){

  new_rcrd( fields = list(path = path, file_type = file_type,
                          year = year, tbl_class = tbl_class,
                          tbl_type = tbl_type, tbl = tbl),
            common_path = common_path,
            class = "adintel_path" )
}

# for compatibility with the S4 system
methods::setOldClass(c("adintel_path", "vctrs_vctr"))

#' `adintel_path` vector
#'
#' This creates a vector of paths for the adbuildr_ad dataset.
#'
#' @param x
#'  * For `adintel_path()`: A character vector of file paths or a sinbgle string indicating a directory.#'.
#'  * For `is_adintel_path()`: An object to test.
#' @return An S3 vector of class `adintel_path`.
#' @export
ad_path <- function(x){

  x <- vctrs::vec_cast(x, character())
  x <- fs::fs_path(x)

  if ( purrr::is_scalar_character(x) ) {
    if ( fs::is_dir(x) ) { x <- get_files(x) }
  }

  possibly_wrangle <- purrr::possibly(wrangle_path, otherwise = list(NULL))

  out0 <- lapply(x, wrangle_path,
                 df_out = TRUE)
  out0 <- do.call("rbind", out0) |>
    subset(!is.na(tbl))


  out <- df_list(out0)

  cmn <- fs::path_common(out$path)

  new_ad_path(path = out$path,
                   file_type = out$file_type,
                   year = out$year,
                   tbl_class = out$tbl_class,
                   tbl_type = out$tbl_type,
                   tbl = out$tbl,
                   common_path = cmn)
}
#'
#' @export
#' @rdname class_adintel_path
is_adintel_path <- function(x) {
  inherits(x, "adintel_path")
}
#'
#' @export
#' @rdname class_adintel_path
format.adintel_path <- function(x, ...) {
  out <- field(x, "path")  |>
    purrr::map_chr( ~ path_split2(.x) |>
                     get_element(-seq_len(3)) |>
                     fs::path_join() )

  type <- field(x, "file_type")

  paste0("[", type, "]", "...", out)
}
#'
#' @export
#' @rdname class_adintel_path
obj_print_footer.adintel_path <- function(x, ...) {
  cat("# ", paste0(attr(x, "common_path"), "/..."), "\n",  sep = "")
}
#'
#' @export
#' @rdname class_adintel_path
vec_ptype_abbr.adintel_path <- function(x, ...) "ad_path"
#' @export
#' @rdname class_adintel_path
vec_ptype_full.adintel_path <- function(x, ...) "adintel_path"

#' @export
#' @rdname class_adintel_path
vec_ptype2.adintel_path.adintel_path <- function(x, y, ...) {
  vec_data(x)$path |> c(vec_data(y)$path) |> ad_path()
}

#' @export
#' @rdname class_adintel_path
vec_ptype2.character.vctrs_percent <- function(x, y, ...) character()

#' @export
#' @rdname class_adintel_path
vec_cast.adintel_path.adintel_path <- function(x, to, ...) x
#' @export
#' @rdname class_adintel_path
vec_cast.adintel_path.character <- function(x, to, ...) ad_path(x)
#' @export
#' @rdname class_adintel_path
vec_cast.character.adintel_path <- function(x, to, ...) field(x, "path")

