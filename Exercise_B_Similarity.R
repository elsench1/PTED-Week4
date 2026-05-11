library(readr)
library(ggplot2)
library(dplyr)
library(sf)
library(SimilarityMeasures)
library(tidyr)

FILE <- "Data/pedestrian.csv"

pedestrian <- read_delim(FILE, delim = ",")

str(pedestrian)
summary(pedestrian)


pedestrian <- pedestrian |>
  arrange(TrajID, DatetimeUTC)

ggplot(pedestrian, aes(x = E, y = N)) +
  geom_path(aes(group = TrajID), colour = "grey70") +
  geom_point(aes(colour = factor(TrajID)), size = 1.5) +
  facet_wrap(~ TrajID, ncol = 3, labeller = label_both) +
  coord_equal() +
  theme_minimal() +
  labs(
    x = "E",
    y = "N",
    colour = "TrajID"
  )



traj <- pedestrian |>
  arrange(TrajID, DatetimeUTC) |>
  group_by(TrajID) |>
  summarise(
    coords = list(as.matrix(data.frame(E = E, N = N))),
    .groups = "drop"
  )

traj1_list <- traj |>
  filter(TrajID == 1) |>
  pull(coords)

traj1 <- traj1_list[[1]]

# str(traj$coords)
# dim(traj1)
# head(traj1)

similarity_results <- traj |>
  filter(TrajID != 1) |>
  rowwise() |>
  mutate(
    DTW = DTW(traj1, coords),
    EditDist = EditDist(traj1, coords, pointDistance = 20),
    Frechet = Frechet(traj1, coords),
    LCSS = LCSSRatio(
      traj1, coords,
      pointSpacing = 10,
      pointDistance = 20,
      errorMarg = 5
    )
  ) |>
  ungroup() |>
  select(TrajID, DTW, EditDist, Frechet, LCSS)

similarity_results


# Most similar according to distance measures: lowest value
similarity_results |>
  arrange(DTW)

similarity_results |>
  arrange(EditDist)

similarity_results |>
  arrange(Frechet)

# Most similar according to LCSS: highest value
similarity_results |>
  arrange(desc(LCSS))

# Interpretation:
# For DTW, EditDist and Frechet, smaller values indicate higher similarity.
# For LCSS, larger values indicate higher similarity.
# Overall, trajectory 6 is most similar to trajectory 1, because it has the
# lowest DTW and EditDist values and the highest LCSS value.
# The Frechet distance suggests trajectory 2 is also very similar.

similarity_long <- similarity_results |>
  pivot_longer(
    cols = -TrajID,
    names_to = "measure",
    values_to = "value"
  )

ggplot(similarity_long, aes(x = factor(TrajID), y = value)) +
  geom_col() +
  facet_wrap(~ measure, scales = "free_y") +
  theme_minimal() +
  labs(
    x = "Compared trajectory",
    y = "Value",
    title = "Similarity to trajectory 1"
  )