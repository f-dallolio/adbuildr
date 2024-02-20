clan_path_sep <- function(x) gsub("[/]{1,}", "/", x)

get_files  <- purrr::partial(fs::dir_ls,
                             recurse = TRUE,
                             type = "file")

clean_split_name <- purrr::compose(
  snakecase::to_snake_case,
  purrr::partial(vslice, i = -1:-3),
  unlist,
  fs::path_split,
  fs::path_ext_remove
)

to_tibble <- purrr::compose(
  purrr::compose(tibble::as_tibble, as.list),
  purrr::partial(rlang::set_names,
                 nm = c('year', 'tbl_class', 'tbl_type')),
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







x <-get_files('/mnt/sata_data_1/adintel/ADINTEL_DATA_2010/')
df <- purrr::map(x, ~ .x |> clean_split_name() |> to_tibble()) |>
  purrr::list_rbind() |>
  # dplyr::mutate(tbl_type = rename_tbl_types(tbl_type)) |>
  dplyr::filter(
    is_numeric_chr(year),
    rev(is_numeric_chr(tbl_class))
  )


not <- function(x) !x

is_numeric_chr('123') |> not()


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

ff <- purrr::partial(fn, x = df$tbl_type, y = df$tbl_class)

ff()


tibble::tibble(
  pattern = c("imp_", "ue_"),
  replacement = c("imp__", "ue__")
) |> map2()


fn2 <- function(x, y, pattern, replacement){
  if(y == "occurrences"){

  }stringr::str_replace_all(x, pattern, replacement)
}

ff <- purrr::partial(fn, x = df$tbl_type)

tibble::tibble(
  pattern = c("imp_", "ue_"),
  replacement = c("imp__", "ue__")
) |> purrr::pmap(ff)


rename_class <- function(.x){
  dplyr::case_when(
    tbl_class == "occurrences" ~ paste0("occ__", tbl_type),
    tbl_class %in% c("impressions",
                     "market_breaks",
                     "universe_estimates",
                     "references") ~ stringr::str_replace_all(
                       stringr::str_split_i(tbl_type, , "_", 1),
                       paste0(stringr::str_split_i(tbl_type, , "_", 1), "_"))
      )
    )

}


x[[1]] |>
  clean_split_name() |> to_tibble()


x <-get_files('/mnt/sata_data_1/adintel/ADINTEL_DATA_2010/')
purrr::map(x, ~ .x |> clean_split_name() |> to_tibble()) |>
  purrr::list_rbind() |>
  dplyr::mutate(tbl_type = rename_tbl_types(tbl_type),
                tbl_type = rename_class(tbl_type, tbl_class))
  # dplyr::mutate(dyn = is_numeric_chr(year) | !is_numeric_chr(tbl_class))
dplyr::filter(
    is_numeric_chr(year) | !is_numeric_chr(tbl_class)
  ) |>
  print(n=Inf)
  check_dynamic(year)
|>
  mutate()|>
  purrr::keep(~ .x$ |> is_tbl_dynamic())

checks <- function(x, )



check_dynamic <- function(x){
  x <- rlang::enquo(x)
  is_numeric_chr(!!x)
}
check_static <- function(x, y){
  !check_dynamic(x) &&
    !is_numeric_chr(y)
}










