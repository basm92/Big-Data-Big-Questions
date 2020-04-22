#libraries
library(tidyverse)
library(stringr)

#Source: World bank
#Pop growth, nuclear energy, forest (% land surface)

data <- c("API_SP.POP.GROW_DS2_en_csv_v2_887477.csv", 
          "API_EG.ELC.NUCL.ZS_DS2_en_csv_v2_890508.csv",
          "API_AG.LND.FRST.ZS_DS2_en_csv_v2_887242.csv")

a <- list()

for (i in 1:length(data)) {
  a[[i]] <- read.csv(data[i], skip = 4)
}

a <- a %>%
  lapply(select, c(-3,-4)) 

#a <- 
a <- a %>%
  lapply(pivot_longer, c(-1,-2), names_to = "year", values_to = "value") %>%
  lapply(mutate, year = str_replace(year, "X", "")) %>%
  lapply(mutate, Country.Name = as.character(Country.Name), Country.Code = as.character(Country.Code))

popgr <- a[[1]]
nucle <- a[[2]]
fores <- a[[3]]

colnames(popgr)[4] <- "pop_growth"
colnames(nucle)[4] <- "nuclear_energy"
colnames(fores)[4] <- "forestation"         

climate <- merge(popgr, nucle) %>%
  merge(fores)

write.csv(climate, "climate.csv")      
