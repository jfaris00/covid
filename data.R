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

data.plot <- data.diff %>%
    gather("country", "diff", -c(date))

gg <- ggplot(data = data.plot %>% filter(country == "China"), aes(x = date, y = diff)) +
    geom_line() 
gg




