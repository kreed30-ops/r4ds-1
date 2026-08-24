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

ggplot(
    billboard_long,
    aes(x = week, y = rank, group = interaction(artist, track))
) +
    geom_line(na.rm = TRUE, alpha = 0.25) +
    scale_y_reverse() +
    scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 76)) +
    labs(
        title = "Billboard Rankings Over Time",
        x = "Weeks on chart",
        y = "Ranking"
    )
#
#
#
