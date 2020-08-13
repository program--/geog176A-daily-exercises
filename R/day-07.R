# Justin Singh-Mohudpur
# 8/12/2020
# Daily Exercise 7

library(tidyverse)
library(ggplot2)
library(ggthemes)

# Get NYTimes COVID-19 Data
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid = read_csv(url)

# Create states data frame
states <- data.frame(
  abb = state.abb,
  state = state.name,
  region = state.region
)

# Join COVID data with states
left_join(covid, states, by = "state") %>%
  replace_na(list(region = "South")) %>% # Handles District of Columbia missing data
  group_by(region, date) %>%
  summarize(cases = sum(cases, na.rm = TRUE), deaths = sum(deaths, na.rm = TRUE)) %>%
  ungroup() ->
  covidByRegion

# Plot
covidByRegion %>%
  pivot_longer(cols = c('cases', 'deaths')) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line(aes(color = region), size = 1.5) +
  facet_grid(name~region, scales = "free_y") +
  labs(title = "Cumulative COVID-19 Cases and Deaths: Region",
       x = "Date",
       y = "",
       caption = "Daily Exercise 07",
       subtitle = "Data Source: NYTimes") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(face = "bold")) +
  theme(axis.text.y = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(color = "navy", face = "bold")) +
  theme(plot.caption  = element_text(color = "gray50", face = "italic")) ->
  ggRegion

# Save plot image
ggsave(ggRegion, file = "img/covid19-region-count.png", height = 8, width = 8)
