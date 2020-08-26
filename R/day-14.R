# Justin Singh-Mohudpur
# 8/25/2020
# Daily Exercise 14

library(tidyverse)
library(sf)
library(USAboundaries)
library(viridis)

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
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      plot.title = element_text(hjust = 0.5, color = "#4e4d47", size = 24),
      panel.background = element_blank(),
      legend.background = element_rect(fill = NA, color = NA),
      panel.border = element_blank(),
      ...
    )
}

get_conus <- function(data, var) {
  conus <- filter(data, !(get(var)) %in% c("Alaska",
                                           "Hawaii",
                                           "Puerto Rico",
                                           "Guam"))
  return(conus)
}

# "id" in use globally as function, changed to "compare" -- works as expected
point_in_polygon <- function(points, polygon, compare) {
  st_join(polygon, points) %>%
    st_drop_geometry() %>%
    count(get(compare)) %>%
    setNames(c(compare, "n")) %>%
    left_join(polygon, by = compare) %>%
    st_as_sf()
}

plot_pip <- function(data) {
  ggplot() +
    geom_sf(
      data = data,
      aes(fill = n),
      alpha = .9,
      size = .2,
      colour = "#2e2e2e") +
    scale_fill_viridis(
      option = "viridis",
      direction = -1,
      name = "Number of Cities",
      trans = "log",
      breaks = c(1, 15, 150, 1500),
      guide = guide_colorbar(
        direction = "horizontal",
        barheight = unit(2, units = "mm"),
        barwidth = unit(50, units = "mm"),
        draw.ulim = F,
        title.position = "top",
        title.hjust = 0.5,
        label.hjust = 0.5)) +
    theme_map() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")) +
    labs(
      title = "Cities in CONUS States",
      caption = paste0(sum(data$n), " locations represented"))
}

conus <- USAboundaries::us_states() %>%
  get_conus("name")

conus_cities <- readr::read_csv("data/uscities.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  get_conus("state_name")

cities_pip <- point_in_polygon(conus_cities, conus, "name") %>%
  plot_pip()

ggsave(cities_pip, file = "img/cities_pip.png", height = 8, width = 8)