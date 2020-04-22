#Olympic Games

library(rvest)
library(tidyverse)
library(stringr)

olympicgames <- read_html("https://en.wikipedia.org/wiki/All-time_Olympic_Games_medal_table") %>%
  html_nodes("table.wikitable:nth-child(11)") %>%
  html_table(fill = TRUE) %>%
  as.data.frame()

summer <- olympicgames[-1,1:6] 
colnames(summer) <- c("Team", "ID", "Gold", "Silver", "Bronze","Total")

summer <- summer %>%
  separate(Team, into = c("Country", "Short"), sep = "\\s\\(", ) %>%
  mutate(Short = str_extract(Short,"[A-Z]{3}"))

summer <- summer[-nrow(summer),]

winter <- olympicgames[-1,c(1,7:11)] 
colnames(winter) <- c("Team", "ID", "Gold", "Silver", "Bronze","Total")

winter <- winter %>%
  separate(Team, into = c("Country", "Short"), sep = "\\s\\(", ) %>%
  mutate(Short = str_extract(Short,"[A-Z]{3}"))

winter <- winter[-nrow(winter),]

write.csv(summer, "olympicgames_summer.csv")
write.csv(winter, "olympicgames_winter.csv")
