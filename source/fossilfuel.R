# World bank - fossil fuel usage
#Source: http://data.worldbank.org/indicator/EG.USE.COMM.FO.ZS/countries

library(readxl)
library(tidyverse)
library(stringr)

fossilfuel <- read.csv("API_EG.USE.COMM.FO.ZS_DS2_en_csv_v2_887511/API_EG.USE.COMM.FO.ZS_DS2_en_csv_v2_887511.csv", skip = 4)

fossilfuel <- fossilfuel %>%
  select(-3,-4) %>%
  pivot_longer(c(-1,-2), names_to = "year", values_to = "usage") %>%
  mutate(year = as.numeric(str_replace(year, "X","")))

write.csv(fossilfuel, "fossilfuel.csv")  

