#libraries
library(tidyverse)
library(stringr)
library(lubridate)

Confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
Deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
Recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

Corona <- list(Confirmed, Deaths, Recovered)

Corona <- Corona %>%
  lapply(pivot_longer, -(1:4), names_to = "date", values_to = "value") %>%
  lapply(mutate, date = mdy(str_replace(date, "X",""))) 
  
Confirmed <- Corona[[1]]
Deaths <- Corona[[2]]
Recovered <- Corona[[3]]

Confirmed <- Confirmed %>%
  group_by(Country.Region, date) %>%
  summarise(value = sum(value))

Deaths <- Deaths %>%
  group_by(Country.Region, date) %>%
  summarise(value = sum(value))

Recovered <- Recovered %>%
  group_by(Country.Region, date) %>%
  summarise(value = sum(value))

Corona <- merge(Confirmed, Deaths, by = c("Country.Region", "date"))
colnames(Corona)[3:4] <- c("Confirmed", "Deaths")

Corona <- merge(Corona, Recovered, by = c("Country.Region", "date"))

colnames(Corona)[5] <- "Recovered"

write.csv(Corona, "coronavirusdata.csv")
