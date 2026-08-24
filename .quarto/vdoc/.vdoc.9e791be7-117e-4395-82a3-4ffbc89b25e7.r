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
#| cache: true
music <- read_csv("data/music.csv", show_col_types = FALSE)
#
#
#
print(names(music))
#
#
#
glimpse(music |> select(starts_with("artist")))
#
#
#
music |>
    select(
        artist.name,
        artist.location,
        artist.latitude,
        artist.longitude,
        artist.terms,
        artist.familiarity,
        artist.hotttnesss,
        song.title,
        song.year
    ) |>
    slice_head(n = 8)
#
#
#
music |>
    summarise(
        across(
            c(artist.familiarity, artist.hotttnesss, song.year, song.tempo),
            list(
                minimum = ~min(.x, na.rm = TRUE),
                first_quartile = ~quantile(.x, 0.25, na.rm = TRUE, names = FALSE),
                median = ~median(.x, na.rm = TRUE),
                third_quartile = ~quantile(.x, 0.75, na.rm = TRUE, names = FALSE),
                maximum = ~max(.x, na.rm = TRUE)
            ),
            .names = "{.col}_{.fn}"
        )
    ) |>
    pivot_longer(
        everything(),
        names_to = c("variable", "statistic"),
        names_pattern = "^(.*)_(minimum|first_quartile|median|third_quartile|maximum)$",
        values_to = "value"
    ) |>
    pivot_wider(names_from = statistic, values_from = value) |>
    select(variable, minimum, first_quartile, median, third_quartile, maximum)
#
#
#
year_values <- music |>
    filter(!is.na(song.year), song.year != 0)

ggplot(year_values, aes(x = song.year)) +
    geom_histogram(binwidth = 5, boundary = 0, color = "white") +
    labs(
        title = "Distribution of Song Years",
        subtitle = paste("Songs used:", nrow(year_values)),
        x = "Year",
        y = "Number of songs"
    )
#
#
#
music |>
    summarise(
        across(
            c(
                artist.location,
                release.name,
                song.title,
                song.year,
                artist.familiarity,
                artist.hotttnesss
            ),
            ~sum(is.na(.x) | as.character(.x) == "0")
        )
    ) |>
    pivot_longer(
        everything(),
        names_to = "column",
        values_to = "placeholder_rows"
    )
#
#
#
music |>
    distinct(artist.id, artist.latitude, artist.longitude) |>
    mutate(
        coordinate_status = if_else(
            artist.latitude == 0 & artist.longitude == 0,
            "Placeholder",
            "Usable"
        )
    ) |>
    count(coordinate_status, name = "artists")
#
#
#
world_map <- maps::map("world", plot = FALSE, fill = TRUE)
world_map <- ggplot2::fortify(world_map)

artist_locations <- music |>
    filter(
        !is.na(artist.latitude),
        !is.na(artist.longitude),
        !(artist.latitude == 0 & artist.longitude == 0),
        !is.na(artist.familiarity),
        artist.familiarity > 0
    ) |>
    mutate(
        genre = case_when(
            str_detect(str_to_lower(artist.terms), "\\brock\\b") ~ "Rock",
            str_detect(str_to_lower(artist.terms), "\\bpop\\b") ~ "Pop",
            str_detect(str_to_lower(artist.terms), "\\bblues\\b") ~ "Blues",
            str_detect(str_to_lower(artist.terms), "\\bjazz\\b") ~ "Jazz",
            str_detect(str_to_lower(artist.terms), "\\bmetal\\b") ~ "Metal",
            TRUE ~ NA_character_
        )
    ) |>
    filter(!is.na(genre)) |>
    distinct(
        artist.id,
        artist.latitude,
        artist.longitude,
        artist.familiarity,
        genre
    )

ggplot() +
    geom_polygon(
        data = world_map,
        aes(x = long, y = lat, group = group),
        fill = "gray90",
        color = "gray60",
        linewidth = 0.2
    ) +
    geom_point(
        data = artist_locations,
        aes(
            x = artist.longitude,
            y = artist.latitude,
            color = genre
        ),
        alpha = 0.45,
        size = 1.2
    ) +
    scale_color_brewer(palette = "Set1", name = "Genre") +
    coord_quickmap() +
    labs(
        title = "Geographic Distribution of Major Artist Genres",
        subtitle = str_wrap(
            "Rock and pop cluster in North America and Europe; blues, jazz, and metal are more dispersed.",
            width = 85
        ),
        x = "Longitude",
        y = "Latitude"
    )
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
