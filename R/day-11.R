# Justin Singh-Mohudpur
# 8/19/2020
# Daily Exercise 11

library(tidyverse)
library(USAboundaries)
library(sf)
library(units)

read_csv("data/uscities.csv") %>%
  filter(city %in% c("Santa Barbara", "Turlock")) %>%
  select(city, state_id, state_name, county_name, lat, lng, population) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) ->
  latlongSBT

latlongSBT %>%
  st_transform(5070) ->
  conusalbersSBT

latlongSBT %>%
  st_transform('+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs') ->
  eqidistanceSBT

latlongDistance = st_distance(latlongSBT)[2,][1]
conusalbersDistance = st_distance(conusalbersSBT)[2,][1]
eqidistanceDistance = st_distance(eqidistanceSBT)[2,][1] %>%
  set_units("km") %>%
  drop_units()

latlongDistance
conusalbersDistance
eqidistanceDistance
