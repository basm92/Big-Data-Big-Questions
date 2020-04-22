library(rvest)
library(tidyverse)
library(stringr)
library(janitor)

pfi <- read_html("https://en.wikipedia.org/wiki/Press_Freedom_Index#Worldwide_Press_Freedom_Index") %>%
  html_nodes("table.wikitable:nth-child(15)") %>%
  html_table(fill = TRUE) %>%
  as.data.frame()

colnames(pfi) <- c("Country", names(pfi[-1]) %>%
  str_extract("[0-9]{4}"))

pfi <- pfi %>%
  pivot_longer(-1, names_to = "year", values_to = "value") %>%
  mutate(rank = str_extract(value, "\\((.+)\\)"), value = str_replace(value, "\\((.+)\\)", "")) %>%
  mutate(rank = as.numeric(str_extract(rank,"[0-9]{3}")), value = as.numeric(value)) 

write.csv(pfi, "worldpressfreedom.csv")
