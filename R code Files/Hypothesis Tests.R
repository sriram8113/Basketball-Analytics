library(tidyverse)
library(lubridate)
library(modi)

#Historically, the Sacramento Kings have been a very poorly performing basketball team.
#The last time the Kings made the NBA playoffs was in the 2005-06 season.
#We are interested in whether the Kings have been so bad compared to the rest of the NBA
#that they should be moved down into the G-league, the NBA's developmental league.
#Our Ha is that the King's performance is more representative of a lower-level league than of an NBA team.


games <- 
    read_csv("gamebygamestats.csv") %>%
    select(HOME_TEAM_ID, VISITOR_TEAM_ID, PTS_home, PTS_away) %>%
    mutate(home_pt_diff = PTS_home - PTS_away,
           away_pt_diff = -1 * home_pt_diff)
#Point differential is used as a metric for performance.

teams <- read_csv("teams.csv")

# conference <- 
#     tibble(team = teams$ABBREVIATION,
#            TEAM_ID = teams$TEAM_ID,
#            conference = c(
#                0,0,1,0,1,1,1,1,1,0,0,1,0,0,0,0,0,1,1,1,1,1,0,1,1,0,0,0,0,1
#            ))
# 

games <-
    bind_rows(select(games, HOME_TEAM_ID, home_pt_diff) %>%
                  `names<-`(c("TEAM_ID", "point_diff")),
              select(games, VISITOR_TEAM_ID, away_pt_diff) %>%
                  `names<-`(c("TEAM_ID", "point_diff")))



kings <- 
    games %>%
    filter(TEAM_ID == filter(teams, ABBREVIATION == "SAC")$TEAM_ID) %>%
    filter(!is.na(point_diff))

non_kings <- 
    games %>%
    filter(TEAM_ID %in% filter(teams, ABBREVIATION != "SAC")$TEAM_ID) %>%
    filter(!is.na(point_diff))

#We are conducting a t-test because we do not have the entire population of NBA game data,
#only that from 2003 to March 2022.

kings_mean <- mean(kings$point_diff)
non_kings_mean <- mean(non_kings$point_diff)
sample_size <- nrow(kings)
sample_var <- var(kings$point_diff)

se <- sample_var / sqrt(sample_size)

t_stat <- (kings_mean - non_kings_mean) / se

pt(t_stat, df = sample_size-1)

#Our chosen alpha would need to be .2443 or greater to reject the null hypothesis that the Kings 
#are not good enough to continue competing in the NBA proper.
#The Kings are historically an abysmally performing NBA team. Therefore, they appear consistently
#in the lower tail of the NBA team performance distribution. However, with a reasonable rejection rate
#we cannot argue that the Kings do not deserve a place in the NBA.


#Are European professional basketball players better at shooting free throws?

#It is common knowledge in the NBA that European-born and -trained players come to the league
#with training focused on differing skills. Often, it is thought that European players
#put more emphasis on the fundamentals of the sport than do others.

#Import dataset that contains free throw data.
player_stats <- read_csv("Player_Seasons_Stats.csv") %>%
    mutate(Player = str_remove_all(Player, "\\*"))
#Import dataset that contains country of origin data.
player_bio <- read_csv("all_seasons.csv")

ft_percs <- 
    player_stats %>% 
    select(Player, Year, FT, FTA) %>%
    group_by(Player) %>%
    nest() %>%
    #Because players have stints of different lengths with different teams,
    #we create a career free-throw percentage by dividing total free throws made
    #by total free throws attempted.
    mutate(career_FT_perc = map_dbl(data, function(x) {
        sum(x$FT) / sum(x$FTA)
            }),
            FTA = map_dbl(data, function(x) sum(x$FTA))) %>%
    select(-data) %>%
    ungroup()


player_countries <- 
    player_bio %>%
    group_by(player_name, country) %>%
    nest() %>%
    select(-data)

player <- 
    inner_join(ft_percs, player_countries, by = c("Player" = "player_name"))
#Unfortunately, we do not have a common "player ID" column. Therefore, we must join by player name.
#On rare occasions there are players that share names, but, since these data only go back to 1996,
#we expect these conflations to be minimal.


europe <- c("Portugal",
            "Denmark",
            "Bosnia and Herzegovina",
            "Austria",
            "Bosnia & Herzegovina",
            "Macedonia",
            "Great Britain",
            "Bosnia",
            "Sweden",
            "Italy",
            "Switzerland",
            "United Kingdom",
            "Latvia",
            "Ireland",
            "Scotland",
            "Netherlands",
            "Poland",
            "Czech Republic",
            "Serbia",
            "Spain",
            "Yugoslavia",
            "Greece",
            "Finland",
            "England",
            "Germany",
            "France",
            "Slovenia",
            "Croatia",
            "Serbia and Montenegro",
            "Ukraine",
            "Lithuania")


#T test because we do not have the population of all basketball players, only NBA
#players going back to 1996.
#Here, we use a weighted mean and weighted variance to control for number of free throws attempted.

euro_players <- 
    player %>%
    filter(country %in% europe)

euro_size <- 
    euro_players %>%
    nrow()

euro_var <- 
    euro_players %>%
    summarize(weighted.var(career_FT_perc, w = FTA, na.rm = TRUE)) %>%
    pluck(1,1)

euro_mean <- 
    euro_players %>%
    summarize(weighted.mean(career_FT_perc, w = FTA, na.rm = TRUE))%>%
    pluck(1,1)

non_euro_mean <- 
    player %>%
    filter(!(country %in% europe)) %>%
    summarize(weighted.mean(career_FT_perc, w = FTA, na.rm = TRUE))%>%
    pluck(1,1)

se <- euro_var / sqrt(euro_size)

t_stat <- (euro_mean - non_euro_mean) / se

pt(t_stat, df = euro_size-1)

#Our t-statistic is 11.1. No reasonable value of alpha would prevent us from rejecting Ho.


#Here, instead of using a weighted mean and weighted variance,
#we omit players that shot less than a certain number of free throws.
#The cut off number is arbitrary, but as long as the number of free throws
#attempted is large enough that the free throw percentage is a good enough proxy
#metric for inherent free throw ability, it should do.

#We loop through a sequence of arbitrary cut-offs and view their associated t-statistics.
tstats <- 
    map_dbl(seq(10, 5000, by = 10), function(x) {
        euro_players <- 
            player %>%
            filter(country %in% europe & FTA >= x)
        
        euro_size <- 
            euro_players %>%
            nrow()
        
        euro_var <- 
            euro_players %>%
            summarize(var(career_FT_perc, na.rm = TRUE)) %>%
            pluck(1,1)
        
        euro_mean <- 
            euro_players %>%
            summarize(mean(career_FT_perc, na.rm = TRUE))%>%
            pluck(1,1)
        
        non_euro_mean <- 
            player %>%
            filter(!(country %in% europe) & FTA >= x) %>%
            summarize(mean(career_FT_perc, na.rm = TRUE))%>%
            pluck(1,1)
        
        se <- euro_var / sqrt(euro_size)
        
        t_stat <- (euro_mean - non_euro_mean) / se
        
        t_stat
    })

t_stats_plt <- 
    tibble(t_stat = tstats) %>%
    ggplot(aes(y = t_stat, x = seq(10, 5000, by = 10))) + 
    geom_point() +
    xlab("Minimum Required Free Throws Attempted") +
    ylab("Resulting T Statistic") +
    ggtitle("T Statistic does not stabilize as cutoff increases")
    
ggsave("euro_t_stats.png",
       plot = t_stats_plt,
       width = 1000,
       height = 1000,
       units = "px",
       dpi = 150)

p_values <- 
    map_dbl(seq(10, 5000, by = 10), function(x) {
        euro_players <- 
            player %>%
            filter(country %in% europe & FTA >= x)
        
        euro_size <- 
            euro_players %>%
            nrow()
        
        euro_var <- 
            euro_players %>%
            summarize(var(career_FT_perc, na.rm = TRUE)) %>%
            pluck(1,1)
        
        euro_mean <- 
            euro_players %>%
            summarize(mean(career_FT_perc, na.rm = TRUE))%>%
            pluck(1,1)
        
        non_euro_mean <- 
            player %>%
            filter(!(country %in% europe) & FTA >= x) %>%
            summarize(mean(career_FT_perc, na.rm = TRUE))%>%
            pluck(1,1)
        
        se <- euro_var / sqrt(euro_size)
        
        t_stat <- (euro_mean - non_euro_mean) / se
        
        pt(t_stat, df = euro_size - 1)
    })

tibble(p_value = p_values) %>%
    ggplot(aes(y = p_value, x = seq(10, 5000, by = 10))) + 
    geom_point() +
    xlab("Minimum Required Free Throws Attempted") +
    ylab("Resulting P Value")

#This plot is very confusing. It does not stabilize. The reason for this is likely because
#the sample size of European players drops to an insignificant number by the time
#the cutoff reaches about 2000 attempted shots.


cutoff_sizes <- 
    map_df(seq(10, 5000, by = 10), function(x) {
        euro_n <- 
            player %>%
            filter(country %in% europe & FTA >= x) %>%
            nrow()
        
        non_euro_n <- 
            player %>%
            filter(!(country %in% europe) & FTA >= x) %>%
            nrow()
        
        tibble(euro_n = euro_n, non_euro_n = non_euro_n, cutoff = x)
    })



euro_sizes <- 
    cutoff_sizes %>%
    `names<-`(c("European", "Non-European", "cutoff")) %>%
    pivot_longer(cols = c("European", "Non-European"), names_to = "NBA Cohort") %>%
    ggplot(aes(cutoff, value, color = `NBA Cohort`)) +
    geom_line() +
    xlab("Minimum Required Free Throws Attempted") +
    ylab("n") +
    ggtitle("Cohort size drops dramatically as restriction increases")

ggsave("euro_sizes.png",
       plot = euro_sizes,
       width = 1200,
       height = 1000,
       units = "px",
       dpi = 150)

#As this plot demonstrates, there are proportionally more players that have taken less
#free throw attempts. The problem with running this test is that career free-throw percentage
#is an imperfect proxy for a player's inherent free throw accuracy. There is certainly a correlation between
#free throw ability and career longevity (and therefore free throws attempted).
#Omitting observations distorts the distribution of our sample in addition to reducing
#the sample sizes. The less-skilled players are going to be the first be omitted regardless
#of the cutoff number selected. However, we have no guarantee that the proportion
#of lower-skilled reduction is identical between the two cohorts.
#European players who do not find success in the NBA are going to be more likely
#to pursue a career in the European basketball leagues. In effect, they would be 
#self-selecting out of our dataset at a higher rate than similarly skilled
#say, American players

#Therefore, the jury is still out on this question, and we would need more
#relevant metrics to come to a decision.






#Are 7-footers worse at free throws?
player_heights <- 
    player_bio %>%
    mutate(height = player_height / 2.54 / 12) %>%
    select(player_name, height) %>%
    unique()

height_dat <- 
    right_join(ft_percs, player_heights, by = c("Player" = "player_name")) %>%
    filter(FTA >= 100)

tall <- 
    height_dat %>%
    filter(height >= 7)

other <- 
    height_dat %>%
    filter(height < 7)

tall_size <- nrow(tall)
tall_var <- var(tall$career_FT_perc)
tall_mean <- mean(tall$career_FT_perc)

other_mean <- mean(other$career_FT_perc)

se <- tall_var / sqrt(tall_size)

t <- (tall_mean - other_mean) / se

pt(t, df = tall_size - 1)
#At any reasonable alpha, we can reject the null hypothesis that 7-footers are just
#as good at free throws as other NBA players.










#Assuming the player lasts for an actual career, do players drafted in the 
#first round actually end up being better?

player_bio %>%
    group_by(player_name) %>%
    nest() %>%
    mutate(data = map(data, function(x) {
        x %>%
            arrange(season) %>%
            mutate(year = 1:nrow(x))
    })) %>%
    unnest(cols = c(data)) %>%
    group_by(year) %>%
    summarize(avg_rating = mean(net_rating),
              n = n()) %>%
    ggplot(aes(year, avg_rating)) +
    geom_col() +
    geom_label(aes(label = n))

#we're going to omit years 1-3 and > 16



draft_rating <- 
    player_bio %>%
    arrange(player_name, draft_round, season) %>%
    group_by(player_name, draft_round) %>%
    nest() %>%
    filter(map_lgl(data, function(x) {
        nrow(x) >= 4
    })) %>%
    mutate(career_rating_avg = map_dbl(data, function(x) {
        y <- 
            x %>%
            mutate(year = 1:nrow(x)) %>%
            filter(year >= 4 & year <= 16)
        
        weighted.mean(y$net_rating, w = y$gp)
    })) %>%
    select(-data) %>%
    ungroup()


first_round <- 
    draft_rating %>%
    filter(draft_round == "1")

other <- 
    draft_rating %>%
    filter(draft_round != "1")

first_size <- nrow(first_round)
first_var <- var(first_round$career_rating_avg)
first_mean <- mean(first_round$career_rating_avg)

other_mean <- mean(other$career_rating_avg)

se <- first_var / sqrt(first_size)

t <- (first_mean - other_mean) / se

pt(t, df = first_size - 1)

#At alpha = 5%, we cannot reject the null that career non-first-round draft picks
#are just as good as career first-round draft picks
