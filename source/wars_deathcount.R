# Wars in history
library(rvest)
library(stringr)
library(janitor)
library(tidyverse)

tables <- read_html("https://en.wikipedia.org/wiki/List_of_wars_by_death_toll") %>%
    html_nodes(".wikitable") %>%
    html_table()

map_df(tables, rbind) %>%
    clean_names() %>%
    names()

final <- map_df(tables, rbind) %>%
    clean_names() %>%
    mutate(geom_mean = ifelse(
        !is.na(geometricmean_note_1), 
        geometricmean_note_1, 
            ifelse(!is.na(geometricmean_clarification_needed), 
                   geometricmean_clarification_needed, geometricmean))) %>%
    select(-c(3,8,9)) %>%
    mutate(geom_mean = str_replace_all(geom_mean, ",", "")) %>%
    mutate(geom_mean = str_replace(geom_mean, "\\+", "")) %>%
    mutate(geom_mean = as.numeric(geom_mean))

write.csv(final, "wars_deathcount.csv")
