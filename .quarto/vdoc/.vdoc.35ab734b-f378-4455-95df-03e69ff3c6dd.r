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
music <- read_csv("data/music.csv", show_col_types = FALSE)
print(music)
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

song_summary <- billboard_long |>
    group_by(artist, track) |>
    summarise(
        first_rank = first(rank[!is.na(rank)]),
        best_rank = min(rank, na.rm = TRUE),
        first_week_at_best_rank = min(
            week[rank == min(rank, na.rm = TRUE)],
            na.rm = TRUE
        ),
        total_weeks = sum(!is.na(rank)),
        .groups = "drop"
    )

top_10_songs <- billboard_long |>
    group_by(artist, track) |>
    mutate(reached_top_10 = any(rank <= 10, na.rm = TRUE)) |>
    ungroup() |>
    filter(reached_top_10) |>
    mutate(
        highlight = case_when(
            artist == "Madonna" & track == "Music" ~ "Madonna - Music",
            artist == "Lonestar" & track == "Amazed" ~ "Lonestar - Amazed",
            artist == "Creed" & track == "Higher" ~ "Creed - Higher",
            TRUE ~ NA_character_
        )
    )

ggplot(top_10_songs, aes(x = week, y = rank,
                         group = interaction(artist, track))) +
    geom_line(color = "gray75", alpha = 0.6, linewidth = 0.5,
              na.rm = TRUE) +
    geom_line(
        data = filter(top_10_songs, !is.na(highlight)),
        aes(color = highlight), linewidth = 1, na.rm = TRUE
    ) +
    scale_color_manual(
        name = "Notable songs",
        values = c(
            "Madonna - Music" = "#0072B2",
            "Lonestar - Amazed" = "#D55E00",
            "Creed - Higher" = "#009E73"
        )
    ) +
    scale_y_reverse() +
    scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 76)) +
    labs(
        title = "Top-10 Billboard Songs Over Time",
        x = "Weeks on chart",
        y = "Ranking"
    )
#
#
#
