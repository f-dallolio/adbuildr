library(tidyverse)
library(adbuildr)
library(arrow)
library(rlang)
library(hms)
library(bit64)
#
# adbuildr::arrow_conversion_new2 |> print(n=Inf)
# nms <- nms2 <- snakecase::to_snake_case(names(arrow::Type)[-1])
# str_replace_all(nms, "_[0-9]", "")
#
# for(i in as.character(0:9)){
#   nms <- str_replace_all(nms, paste0("_", i), i)
# }
# adbuildr::arrow_conversion_new2 |>
#   filter(col_type_name %in% nms)
#
# adbuildr::arrow_conversion_new2$arg_default |>
#   names() |> lapply(possibly(call2))
#
# x <- adbuildr::arrow_conversion_new2$arg_default
# adbuildr::arrow_conversion_new2$arg_default |> discard(is_empty)
#
# c <- x['decimal128'][[1]]
#
# x.1 <- paste0("as.",unique(adbuildr::arrow_conversion_new2$col_class))
# x_1 <- paste0("as_",unique(adbuildr::arrow_conversion_new2$col_class))
#

basetypes <- c("logical", "double", "integer", "character")

is_deprecated <- function(x){
  fbody <- possibly(fn_body)(x)
  # if(x %in% basetypes){
  #   return(FALSE)
  # }
  if(is.null(fbody)){
    return(TRUE)
  } else {
    as.character(fn_body(x)) |> str_detect("deprec") |> any()
  } }

get_as <- function(x){
  x.1 <- paste0('as.', x)
  x_1 <- paste0('as_', x)
  fn <- possibly(get)
  out <- list(x.1, x_1) |>
    set_names(c(x.1, x_1)) |>
    map(fn) |>
    discard(is_deprecated)
  out
}
nms <- adbuildr::arrow_conversion_new2$col_class
# id <- ! nms %in% basetypes
out <- map(.x = nms, get_as) |> set_names(nms)
out
x_as <- map_chr(out, ~ rev(.x)[1] |> names()) |> enframe() |>
  mutate(value = if_else(is.na(value), paste0('as.', name), value)) |>
  set_names(c("col_class", "col_as")) |> distinct()


load("data-raw/base_datatypes.rda")
x <- adbuildr::arrow_conversion_new2 |>
  left_join(base_datatypes) |>
  left_join(x_as) |>
  mutate(col_type_name2 = paste0("new_", col_type_name)) |>
  relocate(col_type_name, col_as, 1)

x <- filter(x, col_class != "data.frame")

fn_out <- vector("list", NROW(x)) |> set_names(x$col_type_name2)



for ( i in seq_along(fn_out) ) {
  print(i)
  base_x <- paste0(x$base_type[[i]],"()" )
  def_call <- x$cll_default[[i]]
  def_call <- call2(call_name(def_call),  .ns = "arrow") |>
    call_modify(splice(call_args(def_call)))
  arg_types <- x$cll_arg_type[[i]]
  if(is_named(x$arg_default[[i]]) && !is_empty(x$arg_default[[i]])){
    xxx <- x$arg_default[[i]]
    if (any(names(xxx) == "x") ) {
      xxx <- xxx[[-which(names(xxx) == "x")]]
    }
  } else{
    xxx <- c("..." = sym(""))
  }
  new_class <- paste0("ad_", x$col_type_name)[[i]]

  if ( x$col_as[[i]] |> str_detect("integer64") ){
    as_x <- call2(x$col_as[[i]], quote(x), .ns = "bit64")
  } else if ( x$col_as[[i]] |> str_detect("hms") ){
    as_x <- call2(x$col_as[[i]], quote(x), .ns = "hms")
  } else {
    as_x <- call2(x$col_as[[i]], quote(x))
  }

  new_name <- x$col_type_name2[[i]]


  nf <- new_function(
    args = c(x = str2lang(base_x), xxx),
                 # if( !is_empty(call_args(def_call))) call_args(def_call),
                 # .ignore_empty = "all"),
    body = expr({
      x <- !!as_x
      vctrs::new_vctr(
        .data = x,
        arrow = !!def_call,
        class = c(class(x), !!new_class)
      )
    })
  )
  fn_out[i] <- list(nf)
  print(new_name)
  print(nf)
  assign(new_name, nf)
}
fn_out
fn__chr <- imap(fn_out, ~ c(paste0(.y, " <- "), capture.output(print(.x))))
xout <- map(fn__chr, ~ c(.x[1:3] |> paste(collapse = ""),
                            .x[-1:-3]))

rd_name <- "ad_type"
rd_tag <- c(
  "#' @export",
  paste("#' @rdname",  rd_name)
)

roxy_header <- header <- c(
  "#' New Data Types",
  "#'",
  paste("#' @name", rd_name),
  "NULL",
  "#'", "#'"
)

out_file <- "data-raw/new_data_types.R"
for(i in seq_along(xout)){
  if( i == 1 ) {
    # if ( file.exists(out_file) ) {
    #   file.remove(out_file)
    #   file.create(out_file)
    # }
    write(roxy_header, out_file)
  }

  new_chr <- xout[[i]][1] |>
    str_split_i("<-", 1) |>
    str_squish()
  new_as <- str_replace_all(new_chr, "new", "as") |> paste0(" <- ", new_chr)

  wrt_out <- c(
    paste0("## (", i,") ", names(xout)[[i]]," -----"),
    rd_tag,
    xout[[i]],
    rd_tag,
    new_as,
    "#'", "#'"
  )

  # wrt_out[str_which(wrt_out, "<- function")] <-
  #   wrt_out[str_which(wrt_out, "<- function")] |>
  #   str_replace_all('int32()', 'integer') |>
  #   str_replace_all('utf8()', 'character')

  n0 <- wrt_out |> str_subset("vctrs::new_vctr") |>
    str_split("") |>
    unlist(recursive = F) |>
    str_count(" ") |>
    rle() |>
    unclass() |>
    pluck("lengths", 1)
  wrt_out <- str_replace_all(wrt_out, ", arrow", paste0(",\n",
                                            str_flatten(rep(" ", n0 + 2)),
                                            " arrow")) |>
    str_replace_all(".data =", paste0("\n",
                                        str_flatten(rep(" ", n0 + 2)),
                                        " .data = "))
  write(wrt_out, out_file, append = TRUE)
}
file.edit(out_file)

# ----
# str_replace_all(", class =", paste0(",\n",
#                                          str_flatten(rep(" ", n0 + 2)),
#                                          " class =")) |>
imap(x$arg_default[[i]], ~paste(dQuote(.y), " = ", dQuote(.x)))

