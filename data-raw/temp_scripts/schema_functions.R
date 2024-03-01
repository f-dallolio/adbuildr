
as_arrow_type <- function(...){
  x <- rlang::dots_list(...)
  do.call(x[[1]], x[-1], envir = asNamespace("arrow"))
}

library(vctrs)
vec_ptype_show(as.integer(runif(10)))

as_vc <- function(x){
  UseMethod(as.vc)
}

new_int32 <- function(x) new_vctr(x, class = c("int32", class(x)), inherit_base_type = TRUE)

convert_to_arrow <- function( x, .class_in = TRUE, .eval = TRUE ){
  if( .class_in ) x <- class(x)[[1]]
  out <- switch(x,
    "integer" = rlang::call2("int32", .ns = "arrow"),
    "integer64" = rlang::call2("int64"), .ns = "arrow",
    "numeric" = rlang::call2("float64", .ns = "arrow"),
    "character" = rlang::call2("utf8", .ns = "arrow"),
    "logical" = rlang::call2("boolean", .ns = "arrow"),
    "Date" = rlang::call2("date32", .ns = "arrow"),
    "hms" = rlang::call2("time32", unit = "s", .ns = "arrow"),
    "duration" <- rlang::call2("duration", unit = "us", .ns = "arrow"),
    "POSIXct" = rlang::call2("timestamp", unit = "us", timezone = ""), .ns = "arrow",
    "list" = rlang::call2("struct", .ns = "arrow")
  )
  if( .eval ){
    eval(out)
  } else {
    out
  }
}

do.call(eval(quote(arrow::decimal)), list(1, 1))

x <- list(what = 'decimal',p =  1, n = 1)
do.call(x[[1]],x[-1], env = asNamespace("arrow"))

?base::`::`


# as_tbl_col_classes <- function(
#     x = c(
#       "integer", "integer64", "numeric", "character",
#       "logical", "Date", "hms", "POSIXct", "list"), .
#     .names){
#   col_classes <- match.arg(x, several.ok = TRUE)
#   col_classes
# }

tbl_schema <- function(x, .input = c("vector", "df"), .output = c("all", "r", "arrow")){
  # input <- match.arg(.input)
  output <- match.arg(.output)
  if( !is.data.frame(x) ){
    arrow::schema( lapply(x, convert_to_arrow) )
  } else {
    out <- setNames(
      sapply(x, function(x) class(x)),
      snakecase::to_snake_case(names(x))
    )
    if(output == "r") {
      out
    } else if (output == "arrow") {
      arrow::schema( lapply(x, convert_to_arrow) )
    } else {
      list(col_classes = out,
           schema = arrow::schema(lapply(x, convert_to_arrow)) )
    }
  }
}

tbl_schema(c(a = "integer"))

schema_to_type <- function(x){
  out <- lapply(as.list(x), purrr::pluck, "type")
  setNames(out, names(x))
}

mtcars2 <- cbind(data.frame(model = rownames(mtcars)),
                 mtcars)
rownames(mtcars2) <- seq_len(NROW(mtcars))
tbl_schema(mtcars2)

adbuildr::adintel_schema$col_classes[[1]] |> tbl_schema() |> schema_to_type()
