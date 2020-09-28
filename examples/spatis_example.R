library(adehabitatHR)

# raw data table
data(bats)

# run SpatIS with SpatialPoints as input

# remove individual with small sample
bats_sp <- bats_sp[bats_sp$Animal_ID != 11,]
#run
spatis(data = bats_sp, individuals.col = "Animal_ID", population.ID = NULL,
       index = "spatis")

spatis(data = bats_sp, individuals.col = "Animal_ID", population.ID = NULL,
       index = c("spatis", "SpatICS"), method = "UDOI", percent = 90)

#---
# Do NOT run, not working for SpatICS
# run SpatIS with sf as input

# remove individual with small sample
bats_sf <- bats_sf[bats_sf$Animal_ID != 11,]
#run
spatis(data = bats_sf, individuals.col = "Animal_ID", population.ID = NULL,
       index = c("spatis", "SpatICS"))

