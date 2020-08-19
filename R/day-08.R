# Justin Singh-Mohudpur
# 8/13/2020
# Daily Exercise 8

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(zoo)

# Get NYTimes COVID-19 Data
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid = read_csv(url)

# Get new daily cases and rolling mean, and plot
covid %>%
  filter(state == "New York") %>%
  group_by(date) %>%
  summarize(cases = sum(cases)) %>%
  mutate(newCases = cases - lag(cases), rollingMean = rollmean(newCases, 7, fill = NA, align="right")) %>%
  ungroup() %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = newCases), col = NA, fill = "#5cd662") +
  geom_line(aes(y = rollingMean), col = "#238f28", size = 1) +
  ggthemes::theme_gdocs() +
  labs(title = "New Reported Cases by Day in New York",
       x = "",
       y = "New Cases",
       subtitle = "Source: NYTimes",
       caption = "Daily Exercise 08") -> ggNY

ggsave(ggNY, file = "img/covid19-newyork-newcases-7daymean.png", height = 8, width = 8)

