library(magrittr)
library(dplyr)
library(sf)
library(readr)

bats <- readr::read_csv("data/spatis_paper_bat_positions.csv",
                        col_types = list(col_character(), col_factor(),
                                         col_double(), col_double(),
                                         col_date(), col_character(),
                                         col_character())) %>%
  dplyr::as_tibble()

bats_sf <- bats %>%
  sf::st_as_sf(coords = c("x", "y"), remove = F,
               crs = "+proj=utm +datum=WGS84 +zone=23 +south +ellps=WGS84 +towgs84=0,0,0")

bats_sp <- as(bats_sf, "Spatial")

save(bats, file = "data/bats.rda")
save(bats_sf, file = "data/bats_sf.rda")
save(bats_sp, file = "data/bats_sp.rda")
