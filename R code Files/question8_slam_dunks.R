library(tidyverse)

dat <- 
    read_csv("nba_pbp.csv") %>%
    mutate(across(contains("description"), ~ if_else(is.na(.x), "", str_to_lower(.x))))


tmp <- 
    dat %>%
    group_by(idGame) %>%
    nest() %>%
    pluck("data") %>%
    `[[`(21) %>%
    mutate(dunk = if_else(str_detect(descriptionPlayHome, "dunk"), "home",
                          if_else(str_detect(descriptionPlayVisitor, "dunk"), "visitor", NA_character_)))
plt <-     
    tmp %>%
    ggplot(aes(minuteGame, marginScore)) +
    geom_point() +
    geom_smooth() +
    geom_vline(data = filter(tmp, !is.na(dunk)), aes(xintercept = minuteGame, color = dunk)) +
    ggtitle("Brooklyn Nets at Philadelphia 76ers: October 23, 2021") +
    ylab("Score Margin") +
    xlab("Game Time (Minutes)")


ggsave("dunk_example.png",
        plot = plt,
        device = "png",
        height = 1500,
        width = 3000,
        units = "px")
