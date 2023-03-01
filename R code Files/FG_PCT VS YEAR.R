library(tidyverse)
library(ggplot2)
library(data.table)

teams <- read.csv("C:/Users/Sriram/OneDrive/Desktop/teams_data_points.csv")
teams

#removing all the 0 values
df1 <- teams[apply(teams!=0, 1, all),]
dim(df1)


#2011
df2011 <- subset(df1, df1$YEAR == ('2011') )
final_data1 <- df2011 %>% select(YEAR, FG_PCT)
#resetting the index values
rownames(final_data1) <- NULL
boxplot(final_data1$FG_PCT)

df2012 <- subset(df1, df1$YEAR == ('2012') )
final_data2 <- df2012 %>% select(YEAR, FG_PCT)
#resetting the index values
rownames(final_data2) <- NULL
boxplot(final_data2$FG_PCT)

df2013 <- subset(df1, df1$YEAR == ('2013') )
final_data3 <- df2013 %>% select(YEAR, FG_PCT)
#resetting the index values
rownames(final_data2) <- NULL
boxplot(final_data2$FG_PCT)

df2014 <- subset(df1, df1$YEAR == ('2014') )
final_data4 <- df2014 %>% select(YEAR, FG_PCT)
#resetting the index values
rownames(final_data4) <- NULL
boxplot(final_data4$FG_PCT)

df2015 <- subset(df1, df1$YEAR == ('2015') )
final_data5 <- df2015 %>% select(YEAR, FG_PCT)
#resetting the index values
rownames(final_data5) <- NULL
boxplot(final_data5$FG_PCT)

df2016 <- subset(df1, df1$YEAR == ('2016') )
final_data6 <- df2016 %>% select(YEAR, FG_PCT)
#resetting the index values
rownames(final_data6) <- NULL
boxplot(final_data6$FG_PCT)

df2017 <- subset(df1, df1$YEAR == ('2017') )
final_data7 <- df2017 %>% select(YEAR, FG_PCT)
#resetting the index values
rownames(final_data7) <- NULL
boxplot(final_data7$FG_PCT)

df2018 <- subset(df1, df1$YEAR == ('2018') )
final_data8 <- df2018 %>% select(YEAR, FG_PCT)
#resetting the index values
rownames(final_data8) <- NULL
boxplot(final_data8$FG_PCT)

df2019 <- subset(df1, df1$YEAR == ('2019') )
final_data9 <- df2019 %>% select(YEAR, FG_PCT)
#resetting the index values
rownames(final_data9) <- NULL
boxplot(final_data9$FG_PCT)

df2020 <- subset(df1, df1$YEAR == ('2020') )
final_data10 <- df2020 %>% select(YEAR, FG_PCT)
#resetting the index values
rownames(final_data10) <- NULL
boxplot(final_data10$FG_PCT)

df2021 <- subset(df1, df1$YEAR == ('2021') )
final_data11 <- df2021 %>% select(YEAR, FG_PCT)
#resetting the index values
rownames(final_data11) <- NULL
boxplot(final_data11$FG_PCT)

plot_data <- rbind(final_data1,final_data2,final_data3,final_data4,final_data5,final_data6,final_data7,final_data8,final_data9,final_data10,final_data11)
plot_data
plot_data1 <- plot_data
rownames(plot_data1) <- NULL
plot_data1

boxplot_year = function(x, y, ylab='', main='') {
  # Boxplot of to observe distribution and moving trend
  # of average team age by year.
  boxplot(split(x, y), main=main, col='green', xlab='Year', ylab=ylab,
          cex.main=2, cex.lab=1.5, cex.axis=1.5)
  # Get a list of league average age by year.
  ms = tapply(x, y, mean)
  # Add a line showing moving average
  lines(ms, col='red', lwd=2, type='o', pch=16)
  # Add points showing moving average
  # points(ms, col='blue')
}



boxplot_year(plot_data1$FG_PCT, plot_data1$YEAR, ylab = 'Field goal percentage',
             main='Boxplot of Field Goal Percentage by Year
	     (Red line shows moving average)')
