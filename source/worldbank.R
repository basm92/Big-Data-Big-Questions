#Some macroeconomic world bank data

#Load query tool
library(tidyverse)
library(stringr)

worldbank <- read.csv("wb.csv")

worldbank <- worldbank %>%
  pivot_longer(-(1:4), names_to = "variable", values_to = "value") %>%
  select(-4) %>%
  mutate(variable = as.numeric(str_extract(variable, "[0-9]{4}"))) %>%
  filter(Series.Name != "") %>%
  pivot_wider(names_from = Series.Name, values_from = value) 

variables <- worldbank[,-(1:2)] %>%
  apply(2, as.numeric) %>%
  as.data.frame()

worldbank <- cbind(worldbank[,(1:2)],variables)
write.csv(worldbank, "worldbank.csv")
