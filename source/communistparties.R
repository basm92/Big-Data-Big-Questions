#Communist coalitions
library(tidyverse)
library(rvest)
library(stringr)

setwd("/home/bas/Documents/Miscprojects/datasetsBQ")

#communistparties <- 
communistparties <- read_html("https://en.wikipedia.org/wiki/List_of_socialist_states") %>%
  html_nodes("table.wikitable:nth-child(23)") %>%
  html_table(fill = TRUE) %>%
  as.data.frame() %>%
  mutate(Country = str_replace(Country, "\\[(.+)\\]", ""), 
         Official.ideology = str_replace(Official.ideology, "\\[(.+)\\]", "")) %>%
  separate(Lower.house, into = c("Lower.house.Communist.seats", "Lower.house.Total.seats"), sep = "\\/") %>%
  separate(Upper.house, into = c("Upper.house.Communist.seats", "Upper.house.Total.seats"), sep = "\\/") 

communistparties <- communistparties %>%
  mutate(Upper.house.Communist.seats = str_extract(Upper.house.Communist.seats, "[0-9]{1,}"),
    Upper.house.Total.seats = str_extract(Upper.house.Total.seats, "[0-9]{1,}"))

write.csv(communistparties, "communistparties.csv")
