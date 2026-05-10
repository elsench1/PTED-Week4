library(sf)
library(dplyr)
library(ggplot2)
library(tmap)

FILE <- "Data/20260425-144523.gpx"

GPS_Track <-
  st_read(FILE, layer = "track_points") |> 
  st_transform(crs = 2056) |> 
  select(
    where(~ {
      if (is.numeric(.)) {
        !all(is.na(.) | . == 0)
      } else {
        !all(is.na(.))
      }
    })
  ) |> 
  select(-any_of(c("speed", "sat", "wrongName")))

# class(GPS_Track$time)

excerpt <- GPS_Track |> 
  filter(
    time >= as.POSIXct("2026-04-25 14:53:50", tz = "Europe/Zurich"),
    time <= as.POSIXct("2026-04-25 16:20:00", tz = "Europe/Zurich")
  )


distance_by_element <- function(later, now) {
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

excerpt <- excerpt |>
  arrange(time) |>
  mutate(
    nMinus2 = distance_by_element(lag(geometry, n = 2), geometry),
    nMinus1 = distance_by_element(lag(geometry, n = 1), geometry),
    nPlus1  = distance_by_element(geometry, lead(geometry, n = 1)),
    nPlus2  = distance_by_element(geometry, lead(geometry, n = 2))
  ) |>
  rowwise() |>
  mutate(
    stepMean = mean(c(nMinus2, nMinus1, nPlus1, nPlus2), na.rm = FALSE)
  ) |>
  ungroup()

# threshold <- 0.1
threshold <- mean(excerpt$stepMean, na.rm = TRUE)

excerpt <- excerpt |>
  mutate(
    static = stepMean <= threshold
  )

excerpt <- excerpt |>
  mutate(
    static = stepMean <= threshold
  )

table(excerpt$static)


ggplot(excerpt) +
  geom_sf(colour = "grey70", size = 0.3) +
  geom_sf(aes(colour = static), size = 1) +
  coord_sf() +
  labs(
    title = "Segmentierte Trajektorie",
    colour = "Static"
  ) +
  theme_minimal()

# tmap_providers()

tmap_options(basemap.server = "SwissFederalGeoportal.NationalMapColor")

tmap_mode("view")

tm_shape(excerpt) +
  tm_dots(
    fill = "static",
    fill.scale = tm_scale(
      values = c("FALSE" = "blue",
                 "TRUE" = "red")
    ),
    size = 0.5
  )



# tmap_options_reset()

rle_id <- function(vec) {
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times = x))
}


excerpt <- excerpt |>
  mutate(segment_id = rle_id(static))

ggplot(excerpt) +
  geom_sf(aes(colour = segment_id), size = 1) +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "Segments",
    colour = "Segment ID"
  )

segment_summary <- excerpt |>
  st_drop_geometry() |>
  group_by(segment_id, static) |>
  summarise(
    start_time = min(time),
    end_time = max(time),
    duration_min = as.numeric(difftime(end_time, start_time, units = "mins")),
    n_points = n(),
    .groups = "drop"
  )

long_segments <- segment_summary |>
  filter(duration_min >= 1.5)

excerpt_long <- excerpt |>
  filter(segment_id %in% long_segments$segment_id)

ggplot(excerpt_long) +
  geom_sf(aes(colour = segment_id), size = 1) +
  coord_sf() +
  theme_minimal()

moving_segments <- excerpt |>
  filter(static == FALSE)

ggplot(moving_segments) +
  geom_sf(aes(colour = segment_id), size = 1) +
  coord_sf() +
  theme_minimal()
