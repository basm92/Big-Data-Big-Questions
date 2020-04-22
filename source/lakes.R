#Largest lakes in Europe
library(rvest)
library(tidyverse)
library(stringr)
library(janitor)

lakes <- read_html("https://en.wikipedia.org/wiki/List_of_largest_lakes_of_Europe") %>%
  html_nodes(".wikitable") %>%
  html_table(fill = TRUE) %>%
  as.data.frame() %>%
  clean_names()

colnames(lakes)[1:2] <- c("overall_rank", "rank_by_subregion")
write.csv(lakes, "lakes.csv")

setwd("/home/bas/Documents/Miscprojects/datasetsBQ")
