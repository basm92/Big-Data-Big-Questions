library(rvest)
library(janitor)
library(tidyverse)

tables <- read_html("https://en.wikipedia.org/wiki/Demographics_of_India") %>%
    html_nodes("body .wikitable") %>%
    html_table(fill = TRUE)

names <- read_html("https://en.wikipedia.org/wiki/Demographics_of_India") %>%
    html_nodes("body .wikitable caption") %>%
    html_text()

populationofindia <- tables[[1]]
colnames(populationofindia) <- populationofindia[1,]
populationofindia <- populationofindia %>%
    .[-1,] %>%
    clean_names() %>%
    mutate_if(is.character, str_replace_all, ",","") %>%
    mutate_at(vars(c(-1,-14)), as.numeric)

populationbystateindia <- tables[[8]] %>%
    select(-1) %>%
    mutate_at(vars(-1), str_replace_all, ",", "") %>%
    mutate_at(vars(-1), as.numeric)

tables[[10]] %>%
    tibble() %>%
    mutate_at(vars(-1), str_replace_all, "%","") %>%
    mutate_at(vars(-1), as.numeric) %>%
    pivot_longer(-1, "year", "share") %>%
    mutate(year = str_extract(year, "[0-9]{4}")) %>%
    pivot_wider(names_from = Religiousgroup, values_from = "value") -> religionindia

demographicdataindia <- tables[[11]] %>%
    tibble() %>%
    mutate_at(vars(-1), str_replace_all, "%","") %>%
    mutate_at(vars(-1), as.numeric) %>%
    clean_names()

hello <- list(demographics = demographicdataindia, 
              populationbystate = populationbystateindia, 
              population = populationofindia,
              religion = religionindia)

i <- NULL
for(i in names(hello)) {
    write.csv(hello[[i]], paste0("india_",i, ".csv", sep = ""))
}

getwd()

