library(sf)
library(dplyr)

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
    time >= as.POSIXct("2026-04-25 14:55:00", tz = "Europe/Zurich"),
    time <= as.POSIXct("2026-04-25 15:17:00", tz = "Europe/Zurich")
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