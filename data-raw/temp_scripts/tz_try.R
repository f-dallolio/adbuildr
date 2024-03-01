

str(OlsonNames()) ## typically around six hundred names,
## typically some acronyms/aliases such as "UTC", "NZ", "MET", "Eire", ..., but
## mostly pairs (and triplets) such as "Pacific/Auckland"
table(sl <- grepl("/", OlsonNames()))
OlsonNames()[ !sl ] # the simple ones
head(Osl <- strsplit(OlsonNames()[sl], "/"))
(tOS1 <- table(vapply(Osl, `[[`, "", 1))) # Continents, countries, ...
table(lengths(Osl))# most are pairs, some triplets
str(Osl[lengths(Osl) >= 3])# "America" South and North ...


library(tidyverse)


# tibble(tz = OlsonNames()) |>
#   rowwise() |>
#   mutate(chr = as.POSIXct(paste("1970-01-01 00:00:00", tz)),
#          dttm = with_tz(chr, tz))
#
#
# |>
#   mutate(date_time = as.POSIXlt(0, "UTC")) |>
#   rowwise() |>
#   mutate(date_time2 = as.POSIXlt(0, tz)) |> print(n=Inf)
# |>
#   pluck(2) |> map_dbl(~ unclass(.x))
# |> as.POSIXlt())
#
#   with_tz()
#   # time_zones <-
  xx <- map(OlsonNames(), ~ compose(as.data.frame,
                                            unclass,
                                            possibly(as.POSIXlt, NA))
                    (0, tz = .x)) |>
    set_names(OlsonNames()) |>
    enframe() |>
    # mutate()
    unnest(everything()) |>
    select(name, zone, gmtoff) |>
    distinct()

  sign <- sign(xx$gmtoff)

  sign[sign == 1] <-  "+"
  sign[sign == -1] <-  "-"
  sign[sign == 0] <-  " "
  h <- (xx$gmtoff/3600)%/%1
  hh <- h*60
  m <- (xx$gmtoff/60)%/%1
  mm <- m - hh
  mm <- str_pad(mm, width = 2, side = "left", pad = "0")
  hh <- str_pad(abs(h), width = 2, side = "left", pad = "0")
  off <- paste0("[",sign, hh, mm, "]")
  off[xx$gmtoff == 0] <- "[ GMT ]"
  xx$gmtoff_min <- xx$gmtoff / 60
  xx$gmtoff_hrs <- xx$gmtoff / 3600
  xx$off <- off

    do.call("rbind",
            map(keep(strsplit(xx$name, "/"), ~length(.x) > 1), `[`, 1:2))


  xxx <- xx |>
    mutate(name1 = str_split_i(name, "/", 1),
           name2 = str_split_i(name, "/", 2))


  nest(mutate(xxx, flag = !is.na(name2)), .by = flag) |>
    filter(flag) |>
    unnest(data) |>
    nest(.by = name1)
  1|>

    nest(.)
    print(n = Inf)
    `select(zone, gmtoff)
  |>
    arrange(gmtoff) |>
    nest(.by = !name)
  |>
    # mutate(zone = map(data,~ .x |> pull(zone))) |>
    # unnest(zone) |>
    distinct()
  xxx |>
    select(zone, gmtoff, off) |>
    mutate(nd = str_count(zone, "\\d")) |>
    arrange(gmtoff, nd)
  |>
    mutate(i = row_number(), .by = c(off, nd)) |>
    filter(i == 1)


  |>
    group_by()


    paste(hh)
  mm










  z <- map(OlsonNames(), ~ compose(as.data.frame,
                              unclass,
                              possibly(as.POSIXlt, NA))
      (0, tz = .x)) |>
    list_rbind() |>
    mutate(name)
  names(z) <- OlsonNames()

  >
    set_names(OlsonNames())
                |>
    modify_if(~ length(.x) > 1, ~ .x[2]) |>
    enframe() |>
    unnest(everything())

