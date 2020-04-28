library(rvest)
library(tidyverse)
library(stringr)
library(janitor)

data <- read_html("https://en.wikipedia.org/wiki/List_of_battles_by_casualties") %>%
    html_nodes(".wikitable") %>%
    html_table(fill = TRUE)

data <- data %>%
    lapply(clean_names) %>%
    lapply(rename_at, vars(1), ~ "battle")

data[[1]] <- data[[1]] %>%
    mutate(casualties = str_extract(casualties, "\\s(.+)"),
           casualties = str_replace(casualties, "\\[(.+)\\]", ""), 
           casualties = str_remove_all(casualties, "–(.+)"),
           casualties = str_remove_all(casualties, ","),
           casualties = str_extract(casualties, "(\\d)+"),
           year = str_remove(year, "#"),
           year = str_remove(year, "![0-9]+\\s"), 
           year = gsub("!(.+)#", "", year))

data[[2]] <- data[[2]] %>%
    clean_names() %>%
    mutate(casualties_high_est = 
               str_replace(casualties_high_est, "\\[(.+)\\]", "")) %>%
    mutate(casualties_high_est = 
               str_replace_all(casualties_high_est, c("," = "", "\\+" = ""))) %>%
    mutate(casualties_low_est =
               str_replace_all(casualties_low_est, "\\[(.+)\\]", "")) %>%
    mutate(casualties_low_est =
               str_replace_all(casualties_low_est, c("," = "", "\\+" = ""))) %>%
    mutate(casualties = (
        as.numeric(casualties_low_est) + as.numeric(casualties_high_est))/2) %>%
    select(-c(casualties_high_est, casualties_low_est))

data[[3]] <- data[[3]] %>%
    mutate(casualties = gsub("–", "\\s", casualties),
           casualties = str_extract(casualties, "\\s(.+)"),
           casualties = str_replace_all(casualties, "\\[(.+)\\]", ""),
           casualties = str_remove(casualties, ","),
           casualties = str_extract(casualties, "(\\d)+"),
           year = str_remove(year, "#")) 

data <- data %>%
    purrr::reduce(rbind)

write.csv(data, "../battles_and_deathcounts.csv")

