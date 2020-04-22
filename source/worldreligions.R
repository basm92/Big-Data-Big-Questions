#Source:
#https://correlatesofwar.org/data-sets/world-religion-data

#Convert to easier format
library(tidyverse)

religions <- read.csv("WRP_national.csv") 

religions <- religions %>%
  pivot_longer(-(c(1:3, 82:84)), names_to = "religion", values_to = "adherents")

write.csv(religions, "worldreligions.csv")
               