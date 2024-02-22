#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name class_adbuildr_ad_path
NULL


new_ad_path <- function(path, file_type, year,
                             tbl_class, tbl_type, tbl,
                             common_path){

  new_rcrd( fields = list(path = path, file_type = file_type,
                          year = year, tbl_class = tbl_class, tbl = tbl),
            common_path = common_path,
            class = "adbuildr_ad_path" )
}

# for compatibility with the S4 system
methods::setOldClass(c("adbuildr_ad_path", "vctrs_vctr"))

#' `adbuildr_ad_path` vector
#'
#' This creates a vector of paths for the adbuildr_ad dataset.
#'
#' @param x
#'  * For `adbuildr_ad_path()`: A character vector of file paths or a sinbgle string indicating a directory.#'.
#'  * For `is_adbuildr_ad_path()`: An object to test.
#' @return An S3 vector of class `adbuildr_ad_path`.
#' @export
ad_path <- function(x = character(), ...){

  x <- vec_cast(x, character())
  x <- fs::fs_path(x)

  if ( purrr::is_scalar_character(x) ) {
    if ( fs::is_dir(x) ) { x <- get_files(x) }
  }

  possibly_wrangle <- purrr::possibly(wrangle_path, otherwise = list(NULL))

  out0 <- lapply(x, possibly_wrangle,
                 df_out = TRUE) |>
    remove_old_static() |>
    purrr::list_rbind()

  out <- df_list(out0)

  cmn <- fs::path_common(out$path)

  new_adbuildr_ad_path(path = out$path,
                   file_type = out$file_type,
                   year = out$year,
                   tbl_class = out$tbl_class,
                   tbl_type = out$tbl_type,
                   tbl = out$tbl,
                   common_path = cmn)
}
#'
#' @export
#' @rdname class_adbuildr_ad_path
is_adbuildr_ad_path <- function(x) {
  inherits(x, "adbuildr_ad_path")
}
#'
#' @export
#' @rdname class_adbuildr_ad_path
format.adbuildr_ad_path <- function(x, ...) {
  out <- field(x, "path")  |>
    purrr::map_chr( ~ path_split2(.x) |>
                     get_element(-seq_len(3)) |>
                     path_join() )

  type <- field(x, "file_type")

  paste0("[", type, "]", "...", out)
}
#'
#' @export
#' @rdname class_adbuildr_ad_path
obj_print_footer.adbuildr_ad_path <- function(x, ...) {
  cat("# ", paste0(attr(x, "common_path"), "/..."), "\n",  sep = "")
}
#'
#' @export
#' @rdname class_adbuildr_ad_path
vec_ptype_abbr.adbuildr_ad_path <- function(x, ...) "ad_path"
vec_ptype_full.adbuildr_ad_path <- function(x, ...) "ad_path"

#' @export
vec_ptype2.adbuildr_ad_path.adbuildr_ad_path <- function(x, y, ...) ad_path()
#' @export
vec_ptype2.character.vctrs_percent <- function(x, y, ...) character()

#' @export
vec_cast.adbuildr_ad_path.adbuildr_ad_path <- function(x, to, ...) x
#' @export
vec_cast.adbuildr_ad_path.character <- function(x, to, ...) ad_path(x)
#' @export
vec_cast.character.adbuildr_ad_path <- function(x, to, ...) field(x, "path")
