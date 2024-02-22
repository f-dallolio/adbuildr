# library(fs)
# library(tidyverse)
#
#
# x <- dir_ls("/mnt/sata_data_1/adintel/") |>
#   str_subset("[0-9]{4}_1", negate = TRUE) |>
#   get_files() |>
#   ad_path()
#
# media_2 <- media_types |>
#   select(media_class, media_type_id, media_type, media_category) |>
#   mutate(media_type_id = media_type_id |> set_names(media_type)) |>
#   summarise(across(media_type_id : media_type, list),
#             .by = c(media_class, media_category))
# media_2$media_type_id
#
#
# xx <- vec_data(x) |>
#   as_tibble() |>
#   summarise(across(path: year, list), .by = c(file_type, tbl_class, tbl_type, tbl)) |>
#   filter(file_type != "ref",
#          tbl_type != "market_breaks") |>
#   mutate(media_class = tbl_type |>
#            str_remove_all("spot_")) |>
#   select(-path, -year) |>
#   distinct() |>
# left_join(media_2, by = "media_class") |>
#   relocate(media_type_id, media_type , .before = media_class)
#
#
# adintel_tbl_class_ptype <- xx |>
#   relocate(tbl, 1)
#
# adintel_tbl_class_ptype |>
#   save(file = "data/adintel_tbl_class_ptype.rda")
