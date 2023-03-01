library(tidyverse)
library(ggplot2)

injuries <- read.csv("C:/Users/VARAD/Desktop/Injury_History.csv")
injuries

#checking if there are any NULL values
sum(is.na(injuries))

inj_updated <- subset(injuries, Position %in% c("C", "PG", "PF", "SG", "SF"))
inj_updated

ggplot(data = inj_updated) +
  geom_bar(mapping = aes(x=Team, fill=Position)) +
  theme(axis.text.x = element_text(angle = 90))