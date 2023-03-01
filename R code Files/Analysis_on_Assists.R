df <- read.csv("AST.csv", fileEncoding = "Latin1", check.names = F)
df
na.omit(df)

install.packages("corrplot")
install.packages("sos")
install.packages("dplyr")
install.packages("ggplot2")

library(corrplot)
library(sos)
library(dplyr)
library(ggplot2)
library(usmap)
library(tidyverse)


findFn("select")

#####################Checking For Correlation#############################

df = subset(df, select = -c(1) )
df2 = subset(df, select = -c(1,2) )

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(df2, histogram = TRUE, method = "pearson")
######################Performing Corelation between 2 Columns###############################
x<- df2[,5]
y<- df2[,"AST"]
# Creating the plot
plot(x, y, pch = 19, col = "lightblue",xlab="Assists", ylab="Passes Received")

# Regression line
abline(lm(y ~ x), col = "red", lwd = 3)

text(paste("Correlation:", round(cor(x, y), 2)), x = 125, y = 95)



###################Find Unique values in Teams#############################################

teams = unique(df[c("TEAM")])
teams_vec = unlist(teams, use.names = FALSE)
length(teams_vec)


########################## Mapping on the US Map #################################################
win_vector<-c()
assist_vector<-c()

for (t in 1:30){
  tt<-with(df, sum(W[TEAM == teams_vec[t] ]))
  print(tt)
  win_vector<- append(win_vector,tt)
  
}

for (t in 1:30){
  tt<-with(df, sum(AST[TEAM == teams_vec[t] ]))
  print(tt)
  assist_vector<-append(assist_vector,tt)
}


cut_win_vector<- win_vector[1:15]
cut_teams_vec<-teams_vec[1:15]             
cut_assist_vector<-assist_vector[1:15]
length(win_vector)
length(teams_vec)


################## Map for Assists in 15 different States#######################################################
#  "MEM" "NOP" "MIN" "DET"
#[23] "UTA" "LAL" "MIA" "SAC" "DAL" "BKN" "SAS" "BOS"

team_state2<-c('ak',"az","to","tx","il","fl","or","nc","oh","in","wi","pa","il","ny","ga","ca","co","az","tn","la","mn","mi","ut","ca","fl","ca","tx","ny","tx","ma")
xxx = assist_vector/15 #dividing by 15 members

#Repeated variables
# rep<-c(13,18,24,25,26,27,28,29) 
# rep_ca<-c(16,24,26)
# rep_tx<-c(4,27,29)
# rep_ny<-c(14,28)
# rep_il<-c(5,13)
# rep_az<-c(2,18)
# rep_fl<-c(6,25)

#Tried to Average the states/total teams  but already the graph does it manually
# xxx[26]<-(xxx[16]+xxx[24]+xxx[26])/3
# xxx[16]<-0
# xxx[24]<-0
# 
# xxx[29]<-(xxx[4]+xxx[27]+xxx[29])/3
# xxx[4]<-0
# xxx[27]<-0
# 
# xxx[28]<-(xxx[14]+xxx[28])/2
# xxx[14]<-0
# 
# xxx[13]<-(xxx[5]+xxx[13])/2
# xxx[5]<-0
# 
# xxx[18]<-(xxx[2]+xxx[18])/2
# xxx[2]<-0
# 
# xxx[25]<-(xxx[6]+xxx[25])/2
# xxx[6]<-0

datafinal<-data.frame(team_state2,xxx)
colnames(datafinal) <- c('state','Assists_Provided')

team_state2 <-tolower(team_state2)
plot_usmap(data = datafinal, 
           regions="state", 
           values="Assists_Provided", labels = TRUE,label_color = "White" ) +
  theme(legend.position = "right")+labs(title = "Assists provided by Different states in USA",
                                        subtitle = "Over the 2020 period")

#################### Map for Wins in 15 different States #########################################################

yyy = win_vector/15 #dividing by 15 members
datafinal2<-data.frame(team_state2,yyy)

colnames(datafinal2) <- c('state','Wins_Achieved')

team_state2 <-tolower(team_state2)
plot_usmap(data = datafinal2, 
           regions="state", 
           values="Wins_Achieved", labels = TRUE, label_color = "White",) +
  theme(legend.position = "right")+labs(title = "Wins by Different state in USA",
                                        subtitle = "Over the 2020 period")
