library(ggplot2)

# raw data table
data(bats)
bats

# plot
ggplot(bats_sf) +
  geom_point(aes(x, y, color = as.factor(Animal_ID))) +
  theme_minimal() +
  labs(color = "Animal_ID")

# sf object
data(bats_sf)
bats_sf

# plot
ggplot(bats_sf) +
  geom_sf(aes(color = as.factor(Animal_ID))
  theme_minimal()


