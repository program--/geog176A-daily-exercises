# Justin Singh-Mohudpur
# 8/20/2020
# Daily Exercise 12

library(tidyverse)
library(USAboundaries)
library(sf)

# Map theme edited from:
# https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
# I quite like the default Ubuntu font, so I've used the showtext library to
# automatically download it for use in the theme.
#
# library(showtext)
# font_add_google("Ubuntu")
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      #panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      #panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # plot.background = element_rect(fill = "#f5f5f2", color = NA),
      plot.background = element_blank(),
      plot.title = element_text(hjust = 0.5, color = "#4e4d47", size = 48),
      # panel.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.background = element_blank(),
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

# Get US States data
USAboundaries::us_states() %>%
  filter(!(name %in% c("Puerto Rico", "Alaska", "Hawaii"))) ->
  CONUS

CONUS %>%
  filter(name == "Colorado") ->
  colorado

st_filter(CONUS, colorado, .predicate = st_touches) -> touchesColorado

ggplot() +
  geom_sf(data = CONUS, colour = "#c2c2c2") +
  geom_sf(data = colorado, fill = '#ff7070', colour = "#c2c2c2") +
  geom_sf(data = touchesColorado, fill = '#ff7070', colour = "#c2c2c2", alpha = 0.5) +
  geom_label(data = colorado, aes(label = name, geometry = geometry), stat = "sf_coordinates", fill = NA, label.size = NA, size = 8) +
  geom_label(data = touchesColorado, aes(label = name, geometry = geometry), stat = "sf_coordinates", fill = NA, label.size = NA, size = 8) +
  labs(title = "States touching the border of Colorado") +
  theme_map() ->
  ggCO

ggsave(ggCO, file = "img/states-touching-colorado.png", height = 8, width = 8, dpi = 300)

