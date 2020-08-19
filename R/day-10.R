# Justin Singh-Mohudpur
# 8/18/2020
# Daily Exercise 10

library(tidyverse)
library(USAboundaries)
library(sf)

# Get US States data
USAboundaries::us_states() %>%
  filter(!(name %in% c("Puerto Rico", "Alaska", "Hawaii"))) ->
  CONUS

# Cast and plot CONUS with internal state boundaries
CONUSBoundaries <- st_combine(CONUS) %>%
  st_cast("MULTILINESTRING")

plot(CONUSBoundaries)

# Cast and plot CONUS without internal state boundaries
CONUSDissolved <- st_union(CONUS) %>%
  st_cast("MULTILINESTRING")

plot(CONUSDissolved)
