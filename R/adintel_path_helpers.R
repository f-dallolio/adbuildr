clan_path_sep <- function(x) gsub("[/]{1,}", "/", x)


is_dyn_table <- function(x, .id = "year"){
  fdusr::is_numeric_chr(x[.id])
}
is_static_tbl <- function(x, .id = "year"){
  !fdusr::is_numeric_chr(x[.id])
}
is_old_static <- function(x, .id = "tbl_class"){
  fdusr::is_numeric_chr(x[.id])
}

keep_dyn_tbl <- function(x, .id = "year"){
  purrr::keep(x, ~ is_dyn_table(.x, .id))
}
keep_static_tbl <- function(x, .id = "tbl_class"){
  purrr::keep(x, ~ is_static_tbl(.x, .id))
}
remove_old_static <- function(x, .id = "tbl_class"){
  purrr::discard(x, ~ is_old_static(.x, .id))
}


get_files <- function(dir, .name = "path"){
  x <- dir
  stopifnot("`dir` requires a directry, not a file " = fs::is_dir(x))
  fs::dir_ls(x, recurse = TRUE, type = "file")
}

make_tbl_column <- function(x,  y ){
  y <- snakecase::to_snake_case(y)
  x <- snakecase::to_snake_case(x) |>
    stringr::str_remove_all("imp_") |>
    stringr::str_remove_all("ue_")

  dplyr::case_when(y == "occurrences" ~ paste0("occ__", x),
                   y == "impressions" ~ paste0("imp__", x),
                   y == "market_breaks" ~ paste0("imp__", x),
                   y == "universe_estimates" ~ paste0("ue__", x),
                   y == "references" ~ paste0("ref__", x),
                   y == "latest" ~ paste0("ref__", x),
                   .default = x) |>
    stringr::str_replace_all("network_tv", "national_tv") |>
    stringr::str_replace_all("spot_tv", "local_tv")
}


wrangle_path <- function(x, df_out = FALSE){

  stopifnot("x must be a single string indicating a file path" =
              purrr::is_scalar_character(x) && fs::is_file(fs::fs_path(x)))

  xx <- x |> path_split2_noext() |>
    get_element( -seq_len(3) ) |>
    rlang::set_names(c('year', 'tbl_class', 'tbl_type')) |>
    snakecase::to_snake_case() |>
    as.list()

  xx[3] <- stringr::str_remove_all(xx[3], "imp_") |>
    stringr::str_remove_all("ue_") |>
    stringr::str_replace_all("network_tv", "national_tv") |>
    stringr::str_replace_all("spot_tv", "local_tv")

  if ( !is_old_static(xx) ){
    xx$tbl <- with(xx, make_tbl_column(tbl_type, tbl_class))
    file_type <- xx$tbl |> stringr::str_split_i("_", 1)
  } else {
    xx$tbl <- NA
    file_type <- "-"
  }

  if (is_dyn_table(xx)) {
    xx[1] <- as.integer(xx[1])
  } else {
    xx[1] <- NA_integer_
  }

  out <- c(path = x, file_type = file_type, xx)
  if (df_out) {
    return(as.data.frame(out))
  }

  out

}
