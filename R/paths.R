clan_path_sep <- function(x) gsub("[/]{1,}", "/", x)
not <- function(x) !x

get_files  <- purrr::partial(fs::dir_ls,
                             recurse = TRUE,
                             type = "file")

clean_split_name <- purrr::compose(
  snakecase::to_snake_case,
  purrr::partial(fdusr::vec_slice_pos, i = -1:-3),
  unlist,
  fs::path_split,
  fs::path_ext_remove
)

to_tibble <- purrr::compose(
  purrr::compose(tibble::as_tibble,
                 as.list),
  purrr::partial(rlang::set_names,
                 nm = c('year', 'tbl_class', 'tbl_type'))
  )

check_tbl_dynamic <- function(.x, .col){
  quo <- rlang::enquo(.col)
  dplyr::filter(.x, is_numeric_chr(!!quo))
}


make_tbl_column <- function(x, y){
  dplyr::case_when(y == "occurrences" ~ paste0("occ__", x),
                   # y == "market_breaks" ~ paste0("imp__", x),
                   y == "references" ~ paste0("dref__", x),
                   y == "latest" ~ paste0("ref__", x),
                   .default = x
  ) |>
    stringr::str_replace_all("imp_", "imp__") |>
    stringr::str_replace_all("ue_", "ue__") |>
    stringr::str_replace_all("network_tv", "national_tv") |>
    stringr::str_replace_all("spot_tv", "local_tv")
}





# x <-get_files('/mnt/sata_data_1/adintel/ADINTEL_DATA_2010/')
# df <- purrr::map(x, ~ .x |>
#                    clean_split_name() |>
#                    to_tibble()) |>
#   purrr::list_rbind() |>
#   dplyr::filter(is_numeric_chr(year) |
#                   not(is_numeric_chr(tbl_class))) |>
#   dplyr::mutate(
#     year = dplyr::if_else(not(is_numeric_chr(year)), NA, as.numeric(year)),
#     tbl = make_tbl_column(tbl_type, tbl_class) )
# df
#
#
# load('data/adintel_schema.rda')
# df |>
#   dplyr::left_join(adintel_schema)
#
# adintel_schema

