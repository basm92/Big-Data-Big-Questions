#Women in national parliaments over time
library(rvest)
library(tidyverse)
library(stringr)
#Source: https://en.wikipedia.org/wiki/European_countries_by_percentage_of_women_in_national_parliaments

womeninparliament <- read_html("https://en.wikipedia.org/wiki/European_countries_by_percentage_of_women_in_national_parliaments") %>%
  html_nodes(".sortable") %>%
  html_table(fill = TRUE) %>%
  as.data.frame() %>%
  pivot_longer(-1, names_to = "year", values_to = "percentage") %>%
  mutate(year = as.numeric(str_replace(year, "X", "")))

write.csv(womeninparliament,"womeninparliament.csv")
