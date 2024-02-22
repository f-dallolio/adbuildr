#' Get Table Names
#'
#' @param a vector of paths.
#' @param n number of rows to read.
#' @param snake_case logical. If `TRUE`, strings are formatted with `snakecase::to_snake_case`.
#'
#' @return a character vector.
#' @export
#'
get_tbl_names <- function(x, n = 1000, snake_case = T){
  if(is_adintel_path(x)){
    x <- get_path(x)
  }
  if(snake_case){
    out <- map(x, ~ .x |> data.table::fread(nrows = n) |>
                 names() |>
                 snakecase::to_snake_case() |>
                 str_replace_all("^adv_paren_code$", "adv_parent_code") |>
                 str_replace_all("^brand_varian$", "brand_variant") |>
                 str_replace_all("^produc_id$", "product_id"))
  } else {
    out <- map(x, ~ .x |> data.table::fread(nrows = n) |> names())
  }
  out
}


