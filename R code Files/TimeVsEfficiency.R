library(ggplot2)
library(data.table)
library(fmsb)
library(dplyr)



# reading data using read.csv
data <-read.csv("/Users/Shared/Relocated/Others/Code/Data Science/Academic/STAT 5000/Project/efficiency.csv")
data 

# total missing values in the dataset
sum(is.na(data$CONF_COUNT))

# Teams interested
teams <- list("Celtics","Bulls","Hawks")



hawks <- data[data$TEAM_NAME=="Hawks", ]
hawks_eff <- hawks$Efficiency

lakers <- data[data$TEAM_NAME=="Lakers", ]
lakers_eff <- lakers$Efficiency

bulls <- data[data$TEAM_NAME=="Bulls", ]
bulls_eff <- bulls$Efficiency

h_years <- hawks$YEAR
b_years <- bulls$YEAR
l_years <- lakers$YEAR


plot(h_years, hawks_eff , type = "b", pch = 19, col = "red", xlab = "Years", ylab = "Efficiency",main = "Efficiency of Lakers, Celtics & Bulls over the years")
points(l_years, lakers_eff, col="blue",pch="+",)
points(b_years, bulls_eff, col="dark red",pch="x")

#add a legend in top left corner of chart at (x, y) coordinates = (1, 19)
legend(1,19,legend=c("Hawks","Lakers","Bulls"), col=c("blue","red","black"),
       pch=c("o","+","x"), ncol=1)






