#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
#
#
#
billboard_long <- billboard |>
    pivot_longer(
        cols = starts_with("wk"),
        names_to = "week",
        names_prefix = "wk",
        values_to = "rank"
    ) |>
    mutate(week = as.integer(week))

billboard_long |>
    group_by(artist, track) |>
    summarise(
        re_entered = any(
            cummax(as.integer(is.na(rank))) == 1 & !is.na(rank)
        ),
        .groups = "drop"
    ) |>
    count(re_entered, name = "songs") |>
    mutate(re_entered = if_else(re_entered, "Yes", "No"))
#
#
#
