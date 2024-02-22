
new_ad_tbl_attr <- function(x) {
  # if (!is.character(x)) {
  #   stop("`x` must be a character vector.")
  # nms <- names(x)
  # .x <- x[['tbl']]
  # .attr <- x[setdiff(nms, 'tbl')] |> df_list()

  new_rcrd(fields = x, class = "ad_tbl_info")

}

str_rpad <- function(x, n = 2, max = T){
  if(max){
    str_pad(x, width = max(nchar(x))+n, side = "right", pad = " ")
  } else{
    str_pad(x, width = nchar(x) + n, side = "right", pad = " ")
  }
}
str_lpad <- function(x, n = 2, max = T){
  if(max){
    str_pad(x, width = max(nchar(x))+n, side = "left", pad = " ")
  } else {
    str_pad(x, width = nchar(x) + n, side = "left", pad = " ")
  }
}

x <- dir_ls("/mnt/sata_data_1/adintel/") |>
  str_subset("[0-9]{4}_1.zip$", negate = TRUE) |>
  get_files() |>
  ad_path() |>
  vec_data() |>
  as_tibble() |>
  filter(!is.na(year) & file_type %in% c('imp', 'occ', 'ue')) |>
  pull(path) |>
  ad_path()

xx <- adintel_tbl_class_ptype |> new_ad_tbl_attr()
vec_data(xx[[1]])
format.ad_tbl_info <- function(x){
  media <- snakecase::to_title_case(field(x, 'media_class')) |>
    str_replace_all("National ", "Nat") |>
    str_replace_all("Local ", "Loc") |>
    str_replace_all("Newspaper", "Newsp") |>
    str_replace_all("Internet", "Inter") |>
    str_replace_all("Magazine", "Magzn") |>
    str_replace_all("Outdoor", "Outdr") |>
    str_replace_all("Digital", "Digtl") |>
    str_replace_all("Fsi Coupon", "Coupn") |>
    str_replace_all("Cinema", "Cinem") |>
    str_rpad(n = 0, max = TRUE)
  # media <- paste0(" ", media, " ") |>
  #   str_embrace("s")


  tclass <- field(x, 'tbl_class')
  tclass <- case_when(tclass == "impressions" ~ "Imprs",
                      tclass == "occurrences" ~ "Occur",
                      tclass == "universe_estimates" ~ "UnEst",
                      tclass == "references" ~ "DRef",
                      .default = "SRef")

  glue::glue("<{ tclass }>{ media }") |>
    str_rpad( n = 0)

}

# > print.ad_tbl_info <- function(x){
#     walk(x, function(x){
#         id <- field(x, 'media_type_id') |> unlist()
#         id <- paste0("  ", id, ":")
#         media <- field(x, 'media_type') |> unlist()
#         comma <- str_flatten(
#             paste0(str_pad(id, max(nchar(id)), side = "right"), media),
#             collapse = "\n")
#
#           print(glue::glue(
#               "
#     { field(x, 'tbl_class') } ({ field(x, 'media_class') })
#     { comma }
#
#     "))
#   }
# )}

fullview <- function(x){
  walk(x, function(x){
    id <- field(x, 'media_type_id') |> unlist()
    id <- paste0("  ", id, ":")
    media <- field(x, 'media_type') |> unlist()
    comma <- str_flatten(
      paste0(str_pad(id, max(nchar(id)), side = "right"), media),
      collapse = "\n")

    print(glue::glue(
      "
    { field(x, 'tbl_class') } ({ field(x, 'media_class') })
    { comma }

    "
    ))
  })
}

fullview(xx)
xx
as_tibble_col(xx, "x")
