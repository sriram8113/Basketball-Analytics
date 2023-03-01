# Steven Coyan

# Read in dataset
bs_2021_2022_nba <- read.csv("bs_2021_2022_nba.csv")
# Clean dataset by removing empty columns
bs_cleaned <- bs_2021_2022_nba %>%
  select(-Column.4,-Column.26,-Column.27,-Column.28,-Column.29,-Column.30) %>%
  drop_na()

# Only look at teams in California
bs_cal_teams <- bs_cleaned %>%
  filter(Team %in% c("GSW","LAL","LAC","SAC")) %>%
  select(Team,Outcome,FTM,FTA,FT_Per = FT.)

bs_GSW <- bs_cleaned %>%
  filter(Team == "GSW") %>%
  select(Team,Outcome,FTM,FTA,FT_Per = FT.)
bs_LAL <- bs_cleaned %>%
  filter(Team == "LAL") %>%
  select(Team,Outcome,FTM,FTA,FT_Per = FT.)
bs_LAC <- bs_cleaned %>%
  filter(Team == "LAC") %>%
  select(Team,Outcome,FTM,FTA,FT_Per = FT.)
bs_SAC <- bs_cleaned %>%
  filter(Team == "SAC") %>%
  select(Team,Outcome,FTM,FTA,FT_Per = FT.)

# Find season averages for each team to represent in plots as a dashed veritcal
# or horizontal line
FT_stats_mean <- tibble(Team = c("GSW","LAL","LAC","SAC"),
                         FT_Per = c(mean(bs_GSW$FT_Per),
                                    mean(bs_LAL$FT_Per),
                                    mean(bs_LAC$FT_Per),
                                    mean(bs_SAC$FT_Per)),
                         FTA = c(mean(bs_GSW$FTA),
                                 mean(bs_LAL$FTA),
                                 mean(bs_LAC$FTA),
                                 mean(bs_SAC$FTA)))

# Plot organized to show comparison between FTA and FT%, colored by win or loss.
ggplot(bs_cal_teams, aes(x = FTA, y = FT_Per, color = Outcome)) +
  geom_point() +
  facet_wrap(vars(Team), ncol = 2) +
  geom_hline(data = FT_stats_mean, linetype = "dashed",
             aes(yintercept = FT_Per)) +
  geom_vline(data = FT_stats_mean, linetype = "dashed",
             aes(xintercept = FTA)) +
  labs(x = "Free Throws Attempted", y = "Free Throw %",
       title = "Correlation of Free Throws to Winning: 2021-2022 NBA Season",
       subtitle = "Do an increase of FTA, FT%, or both lead to in increase of wins?",
       caption = "Study of the 4 Teams in California")




