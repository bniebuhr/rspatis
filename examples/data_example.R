library(ggplot2)
library(sf)

# raw data table
data(bats)
bats

# plot
ggplot(bats) +
  geom_point(aes(x, y, color = Animal_ID)) +
  theme_minimal() +
  labs(color = "Animal_ID")

# sf object
bats_sf

# plot
ggplot(bats_sf) +
  geom_sf(aes(color = Animal_ID)) +
  theme_minimal()

# sp
head(bats_sp)

