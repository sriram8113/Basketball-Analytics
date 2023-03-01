library(tidyverse)
library(ggplot2)
library(ggpubr)

# second dataset has TOT (more than one team) so team isn't specified, removing
# these players ~1,000

## FIRST DATASET CLEANING

# Gets first dataset of team win_pct by season
filename1 <- "data/Historical_NBA_Team_Performance.csv"
win_pct_df <- read_csv(filename1, col_names = c("Season", "Team",
                                               "Record", "Win_Pct"))

# Names columns, removes first row that contains col names,
# adds year column in order to filter data by year,
# selects relevant columns
win_pct_df <- win_pct_df %>% 
  select(Season, Team, Record, Win_Pct) %>% 
  mutate(Year = as.numeric(substr(win_pct_df$Season, 1, 4))) %>%
  mutate(Season = paste(Year, Year + 1, sep = "-")) %>%
  slice(-1) %>%
  filter(Year >= 1979) %>%
  select(Season, Team, Record, Win_Pct, Year)

# Sets type of win_pct column to numeric
win_pct_df$Win_Pct <- as.numeric(win_pct_df$Win_Pct)

### ----------------------------------------------------- ###

## SECOND DATASET CLEANING

# Gets second dataset of player stats by year
filename2 <- "data/Player_Seasons_Stats.csv"
players_df <- read_csv(filename2)

# Sets the type to numeric for the age column
players_df$Age <- as.numeric(players_df$Age)
players_df$Year <- as.numeric(players_df$Year)

# Renames Tm col to Team, gets useful cols,
# creates new col "Season" that incorporates both years of each season
# as seasons cross calendar years, filters data to be from 
# 1979-1980 season and above
players_df <- players_df %>%
  rename("Team" = Tm) %>%
  mutate(Season = paste(Year - 1, Year, sep = "-")) %>%
  filter(Year >= 1980, Team != "TOT") %>%
  select(Season, Player, Age, Team) 

players_df

# a tibble that has the avg_age for each team for each season starting in 1979-1980
avg_age_df <- players_df %>% 
  group_by(Season, Team) %>%
  summarise(Avg_Age = mean(Age))

# create a lookup table for NBA abbreviations
lut <- c("ATL" = "Hawks", "BOS" = "Celtics", "CHI" = "Bulls",
         "CLE" = "Cavaliers", "DEN" = "Nuggets", "DET" = "Pistons",
         "GSW" = "Warriors", "HOU" = "Rockets", "IND" = "Pacers",
         "KCK" = "Kings", "LAL" = "Lakers", "MIL" = "Bucks",
         "NJN" = "Nets", "NYK" = "Knicks", "PHI" = "76ers",
         "PHO" = "Suns", "POR" = "Trail Blazers", "SAS" = "Spurs",
         "SDC" = "Clippers", "SEA" = "Supersonics", "TOR" = "Raptors",
         "UTA"= "Jazz", "WSB" = "Bullets", "DAL" = "Mavericks",
         "LAC" = "Clippers", "SAC" = "Kings", "CHH" = "Hornets",
         "MIA" = "Heat", "MIN" = "Timberwolves", "ORL" = "Magic",
         "VAN" = "Grizzlies", "WAS" = "Wizards", "MEM" = "Grizzlies",
         "NOH" = "Hornets", "CHA" = "Bobcats", "NOK" = "Hornets", 
         "OKC" = "Thunder", "BRK" = "Nets", "NOP" = "Pelicans",
         "CHO" = "Hornets"
         )

# Applies lookup table to team column
avg_age_df$Team <- lut[avg_age_df$Team]


### --------------------------------------------------- ###

## JOINING THE DATASETS

# join on Season then Team
full_df <- full_join(win_pct_df, avg_age_df)
full_df <- full_df %>%
  mutate(Decade = cut(Year, breaks = c(0, 1989, 1999, 2009, 2019),
                     labels = c ("1980s", "1990s", "2000s",
                                 "2010s"))) 

# Two rows for the Knicks had NA so this manually inputs 
# the data for the 2015-2106 and 2016-2017 seasons
na_ind <- which(is.na(full_df$Record))

na_add <- tibble(
  Season = c("2015-2016", "2016-2017"),
  Team = c("Knicks", "Knicks"),
  Record = c("32-50", "31-51"),
  Win_Pct = c(0.39, 0.378),
  Year = c(2015, 2016),
  Avg_Age = c(26.75, 26.625),
  Decade = c("2010s", "2010s")
)

full_df[na_ind, 1:7] <- na_add[1:7]

full_df

# write.csv(full_df, "C:\\Users\\casey\\OneDrive\\Documents\\STAT_5000\\Module_1\\data\\avg_age_win_pct.csv",
          # row.names = FALSE)


### --------------------------------------------------- ###

## PLOTS

# Density Plot of Avg_Age for the whole dataset
ggplot(data = full_df, mapping = aes(x = Avg_Age)) +
  geom_density(fill = 'darkorchid') +
  ggtitle("Density Plot: Average Age Distribution for All NBA Teams 1980-2017")

  #geom_vline(xintercept = x_max) +
  #geom_text(aes(x=x_max, label=x_max, y=0.2))

# Density Plot of Win_Pct for the whole dataset
ggplot(data = full_df, mapping = aes(x = Win_Pct, fill='goldenrod')) +
  geom_density()

# Boxplot of Avg_Age for the whole dataset
ggplot(data = full_df, mapping = aes(y = Avg_Age)) +
  geom_boxplot() 
  
# Boxplot of Win_Pct for the whole dataset
ggplot(data = full_df, mapping = aes(y = Win_Pct)) +
  geom_boxplot() 



# Boxplot of Avg_Age by decade
ggplot(data = full_df) +
  geom_boxplot(mapping = aes(x = Decade, y = Avg_Age))

# Violin + Boxplot for Avg_Age by decade
ggplot(data = full_df, mapping = aes(x = Decade, y = Avg_Age, fill = Decade)) +
  geom_violin(trim=TRUE) +
  geom_boxplot(width=0.1) +
  ggtitle("Average Age of NBA Teams by Decade")

# Density Plots of Avg_Age by decade
ggplot(data = full_df, mapping = aes(x = Avg_Age,fill=Decade)) +
  geom_density() +
  facet_grid(~Decade) +
  scale_fill_manual(values = c('darkseagreen', 'darkorange1',
                               'darkslateblue', 'deeppink3')) +
  ggtitle("Density Plots: Average Age Distribution for NBA Teams by Decade")


# Scatterplot of avg_age vs win_pct

# correlation coefficient of overall
cor(full_df$Avg_Age, full_df$Win_Pct)

ggplot(data = full_df, mapping = aes(x=Avg_Age, y=Win_Pct)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + 
  stat_cor(method = "pearson", label.x = 22, label.y = .85) +
  labs(title="Correlation Between NBA Team's Average Age and Win Percentage")


# Scatterplot of avg_age vs win_pct by decade

df_1980 <- full_df %>% filter(Decade == "1980s")
df_1990 <- full_df %>% filter(Decade == "1990s")
df_2000 <- full_df %>% filter(Decade == "2000s")
df_2010 <- full_df %>% filter(Decade == "2010s")

cor_1980 <- cor(df_1980$Avg_Age, df_1980$Win_Pct)
cor_1990 <- cor(df_1990$Avg_Age, df_1990$Win_Pct)
cor_2000 <- cor(df_2000$Avg_Age, df_2000$Win_Pct)
cor_2010 <- cor(df_2010$Avg_Age, df_2010$Win_Pct)

ggplot(data = full_df, mapping = aes(x=Avg_Age, y=Win_Pct)) +
  geom_point() +
  facet_grid(~Decade) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  stat_cor(method = "pearson", label.x = 22, label.y = .93) +
  ggtitle("Correlation Between NBA Team's Average Age and Win Percentage By Decade") +
  theme(plot.title = element_text(size = 12))
?stat_cor
