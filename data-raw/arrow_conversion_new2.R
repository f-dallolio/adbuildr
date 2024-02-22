
a <- arrow_conversion_new$col_type |> map(str2lang) |> map(call_name)
b <- arrow_conversion_new$col_type |> map(str2lang) |> map(call_name) |> map(str2expression) |> map(eval) |> map(fn_fmls) |> map(as.list)
c <- arrow_conversion_new$args

a
b
cll <- map2(a, b, ~ rlang::call2(.x, splice(.y)))


x <- list(
  Date = list(vctrs::new_date, list()),
  POSIXct = list(vctrs::new_datetime, list()),
  difftime = list(vctrs::new_duration, list(0)),
  hms = list(vctrs::new_duration, list(0)),
  factor = list(vctrs::new_factor, list()),
  logical = list(logical, list()),
  double = list(double, list()),
  integer = list(integer, list()),
  integer64 = list(bit64::integer64, list()),
  character = list(character, list()),
  list = list(rlang::new_list, list(0)),
  list_of = list(vctrs::new_list_of, list()),
  data.frame = list(vctrs::new_data_frame, list()))|>
  map(~ do.call(.x[[1]], .x[[2]])) |>
  enframe() |>
  rename(col_class = name)

x2 <- arrow_conversion_new |>
  mutate(cll_nm = unlist(a),
         cll_arg_type = args,
         cll_default = cll |>
           set_names(cll_nm) ,
         arg_default = map(cll_default, call_args) ) |>
  select(-col_type)

arrow_conversion_new2 <- x2 |> transmute(col_type_name, col_class, cll_arg_type, arg_default, cll_default) |>
  filter(!is.na(col_class)) |>
  print(n = Inf) |>
  left_join(x)  |>
  mutate(flag = !map_lgl(value,is.null)) |>
  filter(!is.na(col_class)) |>
  filter(flag) |>
  select(-flag)

arrow_conversion_new2

usethis::use_data(arrow_conversion_new2, overwrite = TRUE)
