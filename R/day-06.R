# Justin Singh-Mohudpur
# 8/11/2020
# Daily Exercise 6

library(tidyverse)
library(ggplot2)
library(ggthemes)

# Get NYTimes COVID-19 Data
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid = read_csv(url)

# Get vector of top 6 states with most COVID-19 cases
topStates <- covid %>%
  filter(date == max(date)) %>%
  group_by(state) %>%
  summarize(cases = sum(cases, na.rm = TRUE)) %>%
  ungroup() %>%
  slice_max(cases, n=6) %>%
  pull(state)

# Get cumulative cases for top 6 states
statesCumulative <- covid %>%
  filter(state %in% topStates) %>%
  group_by(state, date) %>%
  summarize(cases = sum(cases, na.rm = TRUE)) %>%
  ungroup()

# Plot graphs for each state
stateCases <- ggplot(data = statesCumulative, aes(x = date, y = cases)) +
  geom_line(aes(color = state), size = 1.5) +
  labs(title = "Cumulative Case Counts: COVID-19 Pandemic",
       x = "Date",
       y = "Confirmed Cases",
       caption = "Daily Exercise 06",
       subtitle = "Data Source: NYTimes") +
  facet_wrap(~state) +
  ggthemes::theme_gdocs()

# Save plot image
ggsave(stateCases, file = "covid19-case-count.png")

# Get US total case count per day
totalCaseCount <- covid %>%
  group_by(date) %>%
  summarize(cases = sum(cases, na.rm = TRUE)) %>%
  ungroup()

# Plot graph of total cases per day
totalCases <- ggplot(data = totalCaseCount, aes(x = date, y = cases)) +
  geom_col(colour = "dark red", fill = "dark red", alpha = 0.6) +
  geom_line(colour = "dark red", size = 1.5) +
  labs(title = "National Cumulative Case Counts: COVID-19 Pandemic",
       x = "Date",
       y = "Confirmed Cases",
       caption = "Daily Exercise 06",
       subtitle = "Data Source: NYTimes") +
  ggthemes::theme_gdocs()

# Save plot image
ggsave(totalCases, file = "covid-19-national-case-count.png")
