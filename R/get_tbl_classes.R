#' Get Table Classes
#'
#' @param a vector of paths.
#' @param n number of rows to read.
#' @param snake_case logical. If `TRUE`, strings are formatted with `snakecase::to_snake_case`.
#'
#' @return a character vector.
#' @export
#'
get_tbl_classes <- function(x, n = 10000, snake_case = T){
  if(is_adintel_path(x)){
    x <- get_path(x)
  }
    out <- map(x, ~ .x |> data.table::fread(nrows = n) |>
               rename_with(snakecase::to_snake_case))
    nms <- map(out, ~ .x |> names() |>
                 snakecase::to_snake_case() |>
                 str_replace_all("^adv_paren_code$", "adv_parent_code") |>
                 str_replace_all("^brand_varian$", "brand_variant") |>
                 str_replace_all("^produc_id$", "product_id"))
    cls <- map(out, ~ .x |> map_chr( ~ class(.x) |> rev() |> getElement(1)))
    cls
}

