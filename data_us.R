rm(list = ls())
library(tidyverse)
library(lubridate)

country.data <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

country.data <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

dates <- colnames(country.data)[5:length(country.data)] %>% mdy %>% as.Date

data.summ <- country.data %>%
    select(-c(Lat, Long)) %>%
    group_by(`Country/Region`) %>%
    summarize_if(is.numeric, sum)

countries <- data.summ$`Country/Region`

data.transpose <- data.summ %>%
    select(-`Country/Region`) %>%
    as.matrix %>%
    t %>%
    as.data.frame %>%
    as_tibble

colnames(data.transpose)  <-  countries

data.diff <- data.transpose %>%
    purrr::map_df(~ .x - lag(.x, 1)) %>%
    mutate(date = dates) %>%
    filter(date != head(date, 1)) %>%
    select(date, everything()) 

country.data.plot <- data.diff %>%
    gather("country", "diff", -c(date))

state.data <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

dates <- colnames(state.data)[12:length(state.data)] %>% mdy %>% as.Date

data.summ <- state.data %>%
    select(-c(Lat, Long_, Combined_Key, UID, iso2, iso3, code3, FIPS, Admin2, Country_Region)) %>%
    group_by(`Province_State`) %>%
    summarize_if(is.numeric, sum)

states <- data.summ$`Province_State`

data.transpose <- data.summ %>%
    select(-`Province_State`) %>%
    as.matrix %>%
    t %>%
    as.data.frame %>%
    as_tibble

colnames(data.transpose)  <-  states

data.diff <- data.transpose %>%
    purrr::map_df(~ .x - lag(.x, 1)) %>%
    mutate(date = dates) %>%
    filter(date != head(date, 1)) %>%
    select(date, everything()) 

state.data.plot <- data.diff %>%
    gather("country", "diff", -c(date))

all.data.plot <- full_join(state.data.plot, country.data.plot, by = c("country", "date", "diff"))



gg <- ggplot(data = all.data.plot %>% filter(country %in% c("Florida", "Germany", "United Kingdom", "US")) , aes(x = date, y = diff)) + #
    geom_line(aes(color = country)) 
gg


