library(tidyverse)
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)

#import the data set
data <-read_csv("/Users/kaushiknarasimha/Downloads/nba_games_stats_1.csv")
data #9843 rows, 41 columns

##DATASET CLEANING

#checking for duplicated rows and removing them

data <-data[!duplicated(data), ]
data #data with 9840 rows, 41 columns - 3 duplicate rows removed

#checking for duplicated columns and removing them

data <-data[!duplicated(as.list(data))]
data #no duplicate columns found


#select the required parameters from the data set
df <- data %>% select(1,Date,TeamPoints, FreeThrows, FreeThrowsAttempted, FreeThrows.)

df

write.csv(df,"/Users/kaushiknarasimha/Downloads/free_throws_data.csv", row.names = FALSE)

#total points scored in free throws by all teams
ftotal<-sum(df$FreeThrows)
ftotal

#total points scored by all teams
ptotal <-sum(df$TeamPoints)
ptotal

#average free throws made per game by a team
avgftm <- ftotal/nrow(df)
avgftm

#average free throws attempted per game by a team
avgfta <- sum(df$FreeThrowsAttempted)/nrow(df)
avgfta

#percentage of points scored in free throws
per <- (ftotal/ptotal) * 100
per


class(df$Date)
df$Date <- as.Date(df$Date, ,format="%m/%d/%y")
class(df$Date)



#partitioniong the data based on year
df14 <- subset(df,df$Date >= "2014-01-01" & df$Date < "2015-01-01")
df14

df15 <- subset(df,df$Date >= "2015-01-01" & df$Date < "2016-01-01")
df15

df16 <- subset(df,df$Date >= "2016-01-01" & df$Date < "2017-01-01")
df16


df17 <- subset(df,df$Date >= "2017-01-01" & df$Date < "2018-01-01")
df17


df18 <- subset(df,df$Date >= "2018-01-01" & df$Date < "2019-01-01")
df18

#calculating the total points scored off free throws per year

ftm_per_year = c(sum(df14$FreeThrows),sum(df15$FreeThrows),sum(df16$FreeThrows),sum(df17$FreeThrows),sum(df18$FreeThrows))
ftm_per_year


#calculating the total points scored per year
total_points_per_year=c(sum(df14$TeamPoints),sum(df15$TeamPoints),sum(df16$TeamPoints),sum(df17$TeamPoints),sum(df18$TeamPoints))
total_points_per_year

yr <- c(2014,2015,2016,2017,2018)

ft <- data.frame(yr, ftm_per_year, total_points_per_year)
ft
#correlation plot between free throws and total points per game

ggscatter(df, x = "FreeThrows", y = "TeamPoints", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Points scored in Free Throws", ylab = "Total Points", 
          title = "Correlation Plot between total points and the free throw points") 


df2 <- rbind(
  data.frame(yr, "count" = total_points_per_year, "type"="total_points"),
  data.frame(yr, "count" = ftm_per_year, "type"="points in free throws")
)

ggplot(df2, aes(x=yr, y=count, fill=type)) +
  geom_bar(stat="identity") 
