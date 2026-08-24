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

song_summary

notable_songs <- bind_rows(
    song_summary |>
        filter(best_rank == 1) |>
        slice_min(first_week_at_best_rank, n = 1, with_ties = FALSE) |>
        mutate(notable = "Fastest to number one"),
    song_summary |>
        filter(best_rank == 1) |>
        slice_max(first_week_at_best_rank, n = 1, with_ties = FALSE) |>
        mutate(notable = "Slowest to number one"),
    song_summary |>
        filter(best_rank <= 10) |>
        slice_max(total_weeks, n = 1, with_ties = FALSE) |>
        mutate(notable = "Longest run among top-10 songs")
) |>
    select(notable, artist, track, first_rank, best_rank,
           first_week_at_best_rank, total_weeks)

print(notable_songs, width = Inf)
#
#
#
