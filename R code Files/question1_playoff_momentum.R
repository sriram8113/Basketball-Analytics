library(tidyverse)
library(lubridate)

teams <- read_csv("teams.csv")

perf <- read_csv("Historical_NBA_Team_Performance.csv")

games <- read_csv("gamebygamestats.csv")

games <- 
    games %>%
    left_join(
        teams %>%
            select(TEAM_ID, NICKNAME) %>%
            rename("home_team" = "NICKNAME"),
        by = c("HOME_TEAM_ID" = "TEAM_ID")
    ) %>%
    left_join(
        teams %>%
            select(TEAM_ID, NICKNAME) %>%
            rename("away_team" = "NICKNAME"),
        by = c("VISITOR_TEAM_ID" = "TEAM_ID")
    ) %>%
    rename_all(str_to_lower) %>%
    rename("date" = "game_date_est") %>%
    mutate(game_id = as.character(game_id)) %>%
    select(date, game_id, season, home_team, away_team, home_team_wins) %>%
    mutate(date = mdy(date))



#I was trying to programmatically determine when the NBA Playoffs began
#   for each season, but the COVID-affected seasons threw off any patterns.
#I will just find these dates online.
#It's only 18 seasons. Not too many.
# map_df(seq(min(games$season), max(games$season), 1), function(y) {
#     games %>%
#         filter(season == y) %>%
#         {
#             tmp <- .
#             map_df(seq(min(tmp$date), max(tmp$date), 1), function(x) {
#                 numb_of_games <- 
#                     tmp %>%
#                     filter(date == x) %>%
#                     nrow()
#                 
#                 tibble(
#                     date = x,
#                     count = numb_of_games,
#                     season = y
#                 )
#             })
#         } %>%
#         arrange(desc(date)) %>%
#         filter(count == 4 & lead(count) == 0 & lag(count) == 4)
# })



playoff_start <- 
    tibble(
        season = 2003:2020,
        playoff_date = c(
            "4/17/2004",
            "4/23/2005",
            "4/22/2006",
            "4//21/2007",
            "4/19/2008",
            "4/18/2009",
            "4/17/2010",
            "4/16/2011",
            "4/28/2012",
            "4/20/2013",
            "4/19/2014",
            "4/18/2015",
            "4/16/2016",
            "4/15/2017",
            "4/14/2018",
            "4/13/2019",
            "8/17/2020",
            "5/22/2021"
        )
    ) %>%
    mutate(playoff_date = mdy(playoff_date))

games <- 
    games %>%
    filter(season != 2021) %>%
    left_join(
        playoff_start
    ) %>%
    mutate(playoff_game = date >= playoff_date) %>%
    select(-playoff_date) %>%
    mutate(home_team_wins = as.logical(home_team_wins))



teams <- 
    tibble(
        team = unique(games$home_team),
        west = c(0,1,0,1,0,0,1,1,1,1,1,0,0,1,0,0,1,0,0,1,0,0,1,1,1,1,1,0,0,0)
    ) %>%
    mutate(conference = if_else(west == 0, "East", "West"),
           conference = factor(conference)) %>%
    select(-west)


playoff_upsets <- 
map_df(2003:2019, function(year) {
    standings <- 
        games %>%
        filter(season == year & !playoff_game) %>%
        {
            tmp <- .
            map_df(unique(tmp$home_team), function(x) {
                tibble(
                    team = x,
                    games = nrow(filter(tmp, home_team == x | away_team == x)),
                    wins = nrow(filter(tmp, home_team == x & home_team_wins)) +
                        nrow(filter(tmp, away_team == x & !home_team_wins))
                )
            })
        } %>%
        mutate(win_perc = wins / games) %>%
        left_join(teams) %>%
        group_by(conference) %>%
        nest() %>%
        mutate(data = map(data, function(x) {
            x %>%
                arrange(desc(win_perc)) %>%
                `[`(1:8,) %>%
                mutate(seed = 1:8)
        })) %>%
        unnest(cols = c(data))
    
    
    standings$performance <- 
        map_dbl(1:16, function(x) {
            team <- standings$team[x]
            
            teams_played <- 
                games %>%
                filter(season == 2010 & playoff_game & home_team == team) %>%
                count(away_team) %>%
                nrow()
            
            if (standings$seed[x] == 1) {
                if (teams_played == 4) {
                    1
                } else {
                    2
                }
            } else {
                if (standings$seed[x] == 2) {
                    if (teams_played == 4) {
                        3
                    } else {
                        if (teams_played == 3) {
                            1
                        } else {
                            2
                        }
                    }
                } else {
                    if (standings$seed[x] %in% c(3,4)) {
                        if (teams_played > 2) {
                            3
                        } else {
                            if (teams_played == 2) {
                                1
                            } else {
                                2
                            }
                        }
                    } else {
                        if (standings$seed[x] > 4) {
                            if (teams_played > 1) {
                                3
                            } else {
                                1
                            }
                        }
                    } 
                }
            }
        })
    
    standings <- 
        standings %>%
        mutate(performance = if_else(performance == 1, "expected",
                                     if_else(performance == 2, "underperformed", "overperformed")))
    
    
    tibble(
        season = year,
        in_conference_upsets = floor(nrow(filter(standings, performance != "expected")) / 2)
    )
}) 

plt <- 
    playoff_upsets %>%
    ggplot(aes(in_conference_upsets)) +
    geom_histogram(binwidth = 1) +
    xlab("In-Conference Upsets") + 
    ggtitle("NBA Playoffs 2004-2020: Number of Upsets per Year")

ggsave("playoff_upsets.png", plt, width = 1000, height = 1000, units = "px", dpi = 150)